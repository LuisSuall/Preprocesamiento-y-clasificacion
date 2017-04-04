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
                        nrounds = c(1,10,25,50,100,300),
                        eta = c(0.03, 0.15, 0.3),
                        max_depth = c(6, 8, 10),
                        gamma = c(1,0.5),
                        colsample_bytree = c(0.5,0.75,1),
                        min_child_weight = c(1,0.1),
                        subsample = 1
)

accidentes_kaggle_redux <- accidentes_kaggle # [,-c(15)] to remove CARRETERA
accidentes_kaggle_test_redux <- accidentes_kaggle_test # [,-c(15)] to remove CARRETERA

Tune <- train(TIPO_ACCIDENTE~., data = accidentes_kaggle_redux, 
              method = "xgbTree",
              trControl = tr.Control,
              tuneGrid = tr.Grid,
              verbose = TRUE )

prediction <- predict(Tune, accidentes_kaggle_test_redux)

submission.df <- data.frame(Id=1:nrow(accidentes_kaggle_test),Prediction=prediction)
colnames(submission.df) <- c("Id","Prediction")

write.csv(submission.df,"./submission-xgb.csv", quote = FALSE, row.names = FALSE)

#####################
### Random Forest ###
#####################

library(randomForest)

rf.Grid <- expand.grid(.mtry=seq(18,28,2))

tune.model.list <- list()

for(ntree in c(250,300,400,500)){
  print(ntree)
  rf.tune <- train(TIPO_ACCIDENTE~., data = accidentes_kaggle_redux, 
                   method = "rf",
                   trControl = tr.Control,
                   tuneGrid = rf.Grid,
                   ntree = ntree,
                   verbose = TRUE )
  tune.model.list[[toString(ntree)]] <- rf.tune
}

rf.tune <- tune.model.list[1]

prediction <- predict(rf.tune, accidentes_kaggle_test_redux)

submission.df <- data.frame(Id=1:nrow(accidentes_kaggle_test),Prediction=prediction)
colnames(submission.df) <- c("Id","Prediction")
write.csv(submission.df,"./submission-rf.csv", quote = FALSE, row.names = FALSE)
 