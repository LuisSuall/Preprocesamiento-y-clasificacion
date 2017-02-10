library(caret)
library(randomForest)

tr.Control <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
tr.Grid <- expand.grid(mtry = 10) 
rfTune <- train(TIPO_ACCIDENTE~., data = accidentes_kaggle, 
                method = "rf",
                trControl = tr.Control,
                metric = "Kappa",
                ntree = 100,
                tuneGrid = tr.Grid)

prediction <- predict(gbmFit3, newdata = accidentes_kaggle_test)

submission.df <- data.frame(Id=1:nrow(accidentes_kaggle_test),Prediction=prediction)
write.csv(submission.df,"./submission.csv", quote = FALSE, row.names = FALSE)