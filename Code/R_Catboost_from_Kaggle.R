# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats

# catboost documentation reference here:
# https://tech.yandex.com/catboost/doc/dg/concepts/about-docpage/

# Notes:
# 1. You don't have to use caret. There are known issues with
#    the catboost wrapper.
# 2. Handle missing values. I'm using median imputation.
# 3. You don't have to use pca, corr or nzv; If I use them,
#    my primary purpose is to speed things up for this kernel.
# 4. Next steps? Parameter definitions and tuning.
# 5. dogboost when?
# 6. Vote if you like it!

require(catboost)
require(data.table)
require(caret)
require(geosphere)
require(stringr)
require(Metrics) # for MAE

### LOAD AND TWEAK DATA ###

# Don't use scientific notation in output
options(scipen = 999)

#### LOAD AND BASIC SET-UP ####

pr <- fread('../input/properties_2016.csv')

t <- fread('../input/train_2016_v2.csv')

# Drop dupes
t <- t[!duplicated(t$parcelid), ] # These should probably be averaged

t[,transactiondate := NULL] # Extract month later?

# List of logical vectors
logilist <- c(
            "architecturalstyletypeid",
            "hashottuborspa",
            "pooltypeid10",
            "pooltypeid2",
            "typeconstructiontypeid",
            "fireplaceflag"
            )

# Convert logicals to factors
pr[, logilist] <- lapply(pr[, logilist, with=FALSE], factor)

# Convert char to factor
pr$propertycountylandusecode <- as.factor(pr$propertycountylandusecode)
pr$propertyzoningdesc <- as.factor(pr$propertyzoningdesc)
pr$taxdelinquencyflag <- as.factor(pr$taxdelinquencyflag)

# Drop
pr[,censustractandblock := NULL] # Duplicate
pr[,regionidcounty := NULL] # Duplicate

# Considering imputing NAs or just want to speed things up? 
# Drop all vectors with >= x% NA
vectordrop <- pr[, lapply( pr, function(x) sum(is.na(x)) / length(x) ) >= .50 ]

pr[, names(which(vectordrop == TRUE)) := NULL]

imp_values <- preProcess(pr,
                        method = c("medianImpute")
                        )

p <- predict(imp_values, pr)

# Convert int/num to factor
pr$regionidcity <- as.factor(pr$regionidcity)
pr$regionidneighborhood <- as.factor(pr$regionidneighborhood)
pr$regionidzip <- as.factor(pr$regionidzip)
pr$storytypeid <- as.factor(pr$storytypeid)
pr$airconditioningtypeid <- as.factor(pr$airconditioningtypeid)
pr$buildingtypeclassid <- as.factor(pr$buildingtypeclassid)
pr$decktypeid <- as.factor(pr$decktypeid)
pr$fips <- as.factor(pr$fips)
pr$propertylandusetypeid <- as.factor(pr$propertylandusetypeid)

#### ADD SOME FEATURES ####

# Dist from centroid
p$longmean <- mean((p$longitude), na.rm=TRUE)/1e6
p$latmean <- mean((p$latitude), na.rm=TRUE)/1e6

# Adjusted long lat
p$longitude1 <- p$longitude/1e6
p$latitude1 <- p$latitude/1e6

# Haversine distance
ncol(p)
p$geodist <- distHaversine(p[,31:32], p[,33:34])

p[,longmean := NULL]
p[,latmean := NULL]
p[,longitude1 := NULL]
p[,latitude1 := NULL]

# Tax based info
pr[, landValRatio := (pr$landtaxvaluedollarcnt / (pr$landtaxvaluedollarcnt + pr$structuretaxvaluedollarcnt))]

# Bathrooms are important
pr[, bathInteraction := (pr$bathroomcnt * pr$calculatedfinishedsquarefeet)]

# Sq Ft / Room
pr[, sqftRoom := (pr$calculatedfinishedsquarefeet / pr$roomcnt)]

# Struc / Lanad
pr[, strucLand := (pr$calculatedfinishedsquarefeet / pr$lotsizesquarefeet)]

# Age
pr[, age := 2020 - pr$yearbuilt]

#### BUILD MODEL ####

# Create training set
train <- merge(p, t, by="parcelid", all.y=TRUE)

# We need all of the RAM we can get
rm(t)
rm(pr)

p[, logerror := 0]

set.seed(28)

# Enable caret to use MAE as eval metric
maeSummary <- function (train,
                        lev = NULL,
                        model = NULL) {
                            out <- mae(train$obs, train$pred)  
                            names(out) <- "MAE"
                            out
                        }

control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter=TRUE,
                              summaryFunction = maeSummary
                              )

grid <- expand.grid(depth = c(6),
                    learning_rate = c(0.005),
                    iterations = c(600),
                    l2_leaf_reg = c(1e-3),
                    rsm = c(0.95),
                    border_count = c(32)
                    )

traindata <- train[,2:(ncol(train)-1)] #parcelid is column [1], logerror is last col
target <- train[,logerror]

cb <- train(y=target,
            x=traindata, 
            preProcess=NULL,
            method=catboost.caret, 
            metric = "MAE", 
            maximize = FALSE, 
            tuneGrid = grid, 
            trControl = control
            )

print(cb)

importance <- varImp(cb, scale = FALSE)
print(importance)

cb_prediction <- predict(cb, p)

# Lots of people seem to be using constants to adjust
# for systematic overestimation
predictions <- 0.93*cb_prediction + 0.065*0.012

predictions <- round(as.vector(predictions), 4)

result <- data.frame(cbind(p$parcelid, predictions, predictions, predictions, predictions, predictions, predictions))

colnames(result) <- c("parcelid","201610","201611","201612","201710","201711","201712")

write.csv(result, file = "cb-submission.csv", row.names = FALSE ) 

#   /\_/\
#   >^.^<.---.
#  _'-`-'     )\
# (6--\ |--\ (`.`-.
#     --'  --'  ``-' 
# Thanks for looking!
