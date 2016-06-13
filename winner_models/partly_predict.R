table(train$TARGET)


pred <- predict(model, dfull)
head(pred, 20)


Metrics::rmse(train$TARGET, pred)
Metrics::auc(train$TARGET, pred)



table(train[pred > 0.4, 'TARGET'])
View(train[train$TARGET == 0 & pred > 0.3, ])

pred[pred > 0.4] = 1
pred[train$TARGET == 0 & pred > 0.4] = 0



table(train[pred < 0.01, 'TARGET'])
View(train[train$TARGET == 0 & pred > 0.3, ])

pred[train$TARGET == 1 & pred < 0.1] = 1



View(train[train$TARGET == 1 & pred > 0.1, ])

labels2 <- train$TARGET
labels2[train$TARGET == 1 & pred > 0.1] = 0
labels2[train$TARGET == 1 & pred < 0.01] = 0
table(labels2)


feature.names <- names(train)
feature.names <- feature.names[-match(c('ID', 'TARGET'), feature.names)]
# feature.names <- feature.names[-match(c('var15'), feature.names)]
# feature.names <- feature.names[-match(c('saldo_var30'), feature.names)]
# feature.names <- feature.names[-match(c('saldo_medio_var5_hace2'), feature.names)]
feature.formula <- formula(paste('TARGET ~ ', paste(feature.names, collapse = ' + '), sep = ''))

dfull2 <- xgb.DMatrix(data.matrix(train[, feature.names]), label = labels2)

set.seed(seed.number)
model2 <- xgb.train(
  data = dfull2,
  params = list(booster = "gbtree", objective = "binary:logistic",
             eval_metric = "auc",
             max_depth = 5,
             eta = 0.005,
             colsample_bytree = 0.5,
             subsample = 0.7),
  nrounds = 2000)

set.seed(seed.number)
pred2 <- predict(model2, dfull)
# head(pred2, 20)

Metrics::rmse(labels2, pred2)
Metrics::auc(labels2, pred2)


table(labels2[pred2 > 0.05])
View(train[train$TARGET == 1 & pred > 0.4, ])

table(labels2[pred2 < 0.1])
View(train[train$TARGET == 0 & pred > 0.1, ])



pred_mixed <- pred
indexes <- pred > 0.01 & pred < 0.1
pred_mixed[indexes] <- pred2[indexes]


Metrics::rmse(train$TARGET, pred_mixed)
Metrics::auc(train$TARGET, pred_mixed)




set.seed(seed.number)
pred2.test <- predict(model2, dtest)

pred_mixed <- regression_minus_23_33_45_50_25_0_0.013_10$TARGET
indexes <- regression_minus_23_33_45_50_25_0_0.013_10$TARGET > 0.01 & regression_minus_23_33_45_50_25_0_0.013_10$TARGET < 0.1
# table(indexes)
pred_mixed[indexes] <- pred_mixed[indexes]*0.9 + pred2.test[indexes]*0.1


submission <- data.frame(ID = test$ID, TARGET = pred_mixed)
submissionName <- paste("./results/partly_predict",
                        format(Sys.time(), "%H_%M_%S"),
                        0.01, 0.1, 0.9, 0.1,
                        sep = '_')
submissionFile <- paste0(submissionName, ".csv")
write.csv(submission, submissionFile, row.names=FALSE, quote = FALSE)


