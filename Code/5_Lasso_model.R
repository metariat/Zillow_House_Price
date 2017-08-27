library(data.table)



#---------------------------------------------------------------------#
#---------------------------------------------------------------------#
#                 Data reading                                      #
#---------------------------------------------------------------------#
#---------------------------------------------------------------------#

train = setDT(readRDS("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/train.RDS"))
test = setDT(readRDS("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/test.RDS"))


train[, buildingclasstypeid:= NULL]
train[, buildingqualitytypeid:= as.factor(as.character(buildingqualitytypeid))]

test[, buildingclasstypeid:= NULL]
test[, buildingqualitytypeid:= as.factor(as.character(buildingqualitytypeid))]

#-------------------------------------------------------------------#
# Model without outliers #
#-------------------------------------------------------------------#
train = train[abs(train$logerror) < 1.5, ]
y.train = train$logerror
x.train = subset(train, select = -logerror)
x.train = model.matrix(~., x.train)




#Check the redundant variables
l <- lapply(x.train, function(X) as.numeric(factor(X, levels=unique(X))))
m <- as.matrix(data.frame(l))
M <- (cor(m,m)==1)
M[is.na(M)] = FALSE
M[lower.tri(M, diag=TRUE)] <- FALSE
colnames(M)[colSums(M)>0]