library(caret)
library(xgboost)

###############
### XGBoost ###
###############

tr.Control <- trainControl(method = "repeatedcv", 
                           number = 5, 
                           repeats = 1,
                           verboseIter = TRUE)
tr.Grid <- expand.grid(
                        nrounds = c(1,10,25),
                        eta = c(0.01, 0.001, 0.0001),
                        max_depth = c(2, 4, 6, 8, 10),
                        gamma = 1,
                        colsample_bytree = 1,
                        min_child_weight = 1,
                        subsample = 1
)

accidentes_kaggle_redux <- accidentes_kaggle[,-c(7,15)]
accidentes_kaggle_test_redux <- accidentes_kaggle_test[,-c(7,15)]

Tune <- train(TIPO_ACCIDENTE~., data = accidentes_kaggle_redux, 
              method = "xgbTree",
              trControl = tr.Control,
              tuneGrid = tr.Grid,
              verbose = TRUE )

prediction <- predict(Tune, accidentes_kaggle_test_redux)

submission.df <- data.frame(Id=1:nrow(accidentes_kaggle_test),Prediction=prediction)
write.csv(submission.df,"./submission-xgb.csv", quote = FALSE, row.names = FALSE)

#####################
### Random Forest ###
#####################

library(randomForest)

rf.Grid <- expand.grid(.mtry=seq(2,42,4))

tune.model.list <- list()

for(ntree in c(10,25,seq(50,200,50),seq(300,500,100))){
  print(ntree)
  rf.tune <- train(TIPO_ACCIDENTE~., data = accidentes_kaggle_redux, 
                   method = "rf",
                   trControl = tr.Control,
                   tuneGrid = rf.Grid,
                   ntree = ntree,
                   verbose = TRUE )
  tune.model.list[[toString(ntree)]] <- rf.tune
}

#prediction <- predict(rf.tune, accidentes_kaggle_test_redux)

rf.tune <- tune.model.list[7]

submission.df <- data.frame(Id=1:nrow(accidentes_kaggle_test),Prediction=prediction)
write.csv(submission.df,"./submission-rf.csv", quote = FALSE, row.names = FALSE)
