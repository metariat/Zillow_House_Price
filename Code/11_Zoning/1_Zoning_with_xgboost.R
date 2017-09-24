library(data.table)
library(xgboost)
library(caret)
path = "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/"

data = fread(paste0(path, "Cat_residuals.csv"))
data[, residuals := obs - pred]

data[, obs := NULL]
data[, pred := NULL]
data[, insee := NULL]
data[, Weight := NULL]

train = data[random < 9, ]
test = data[random > 8, ]

y.train = train$residuals
x.train = subset(train, select = -c(random, residuals))
x.train = as.matrix(x.train)

y.test = test$residuals
x.test = subset(test, select = -c(random, residuals))
x.test = as.matrix(x.test)




# set up the cross-validated hyper-parameter search
xgb.grid = expand.grid(
  nrounds = 500,
  max_depth = c(2),
  eta = c(0.01, 0.005, 0.02),
  gamma = 0,
  colsample_bytree = c(0.5),
  min_child_weight = c(1, 10, 20),
  subsample = c(0.6, 0.8, 0.7, 0.5)
)


# pack the training control parameters
xgb.trcontrol = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  #classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
  allowParallel = TRUE
)

time1 = Sys.time()
# train the model for each parameter combination in the grid, 
#   using CV to evaluate
xgb.train = train(
  x = x.train,
  y = y.train,
  trControl = xgb.trcontrol,
  tuneGrid = xgb.grid,
  method = "xgbTree",
  metric = "MAE"
)
time2 = Sys.time()


# nrounds = 500, max_depth = 2, eta = 0.02, gamma = 0, 
# colsample_bytree = 0.5, min_child_weight = 10, subsample = 0.5


y_mean = mean(y.train)

param <- list(objective           = "reg:linear",
              eval_metric         = "mae",
              eta                 = 0.01,
              max_depth           = 2, 
              subsample           = 0.5,
              colsample_bytree    = 0.5,
              min_child_weight    = 10,
              gamma               = 0.02,
              base_score          = y_mean)

xgb_cv <- xgb.cv(data = x.train,
                 label = y.train,
                 params = param,
                 nrounds = 500,
                 prediction = TRUE,
                 maximize = FALSE,
                 nfold = 5,
                 print_every_n = 5)

plot(xgb_cv$evaluation_log$train_mae_mean, type = "l")
lines(xgb_cv$evaluation_log$test_mae_mean, type = "l", col = "red")

print(xgb_cv$evaluation_log[which.min(xgb_cv$evaluation_log$test_mae_mean)]) #0.0402418
round = xgb_cv$evaluation_log[which.min(xgb_cv$evaluation_log$test_mae_mean)]$iter

dtrain = xgb.DMatrix(as.matrix(x.train),label = y.train)

model = xgb.train(param, data = dtrain, nrounds = round)
importance <- xgb.importance(colnames(dtrain), model = model)
xgb.plot.importance(importance, rel_to_first = TRUE, xlab = "Relative importance")




#training mae
xgb.pred = predict(model, new = x.train)
mean(abs(xgb.pred - y.train))


#testing mae
xgb.pred = predict(model, new = x.test)
mean(abs(xgb.pred - y.test))

zoning = predict(model, new = x.train)
df = data.frame("X" = data.frame(x.train)$X,
                 "Y" = data.frame(x.train)$Y,
                 "Z" = zoning)

library(plotly)
library(rgl)
library(reshape2)
df.x<-unique(df$X)
df.y<-unique(df$Z)
df.z<-acast(df,X~Z)
plot_ly(x=dd$X,y=dd$Y,z=dd$Z*1000, type="surface")
persp3d(x=dd$X,y=dd$Y,z=dd$Z*1000, col="skyblue")
