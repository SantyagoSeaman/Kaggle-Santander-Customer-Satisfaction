table(train$TARGET)


Metrics::rmse(train$TARGET, pred.full)
Metrics::auc(train$TARGET, pred.full)
table(pred.full > 0.1)


table(train[pred.full > 0.05, 'TARGET'])
table(train[pred.full > 0.1, 'TARGET'])
table(train[pred.full > 0.2, 'TARGET'])
table(train[train$TARGET == 0 & pred.full > 0.1, 'TARGET'])

View(train[train$TARGET == 0 & pred.full > 0.3, ])

pred[pred > 0.4] = 1
pred[train$TARGET == 0 & pred > 0.4] = 0



table(train[pred < 0.01, 'TARGET'])
table(train[pred > 0.01 & pred < 0.1, 'TARGET'])
table(train[pred > 0.1 & pred < 0.2, 'TARGET'])
table(train[pred > 0.2 & pred < 0.3, 'TARGET'])
table(train[pred > 0.3 & pred < 0.4, 'TARGET'])
View(train[train$TARGET == 0 & pred > 0.3, ])

pred[train$TARGET == 1 & pred < 0.1] = 1



View(train[train$TARGET == 1 & pred.full > 0.1, ])

# -----------------------------------------------------------
labels2 <- train$TARGET
labels2[train$TARGET == 1 & pred.full < 0.1] = 0
# labels2[train$TARGET == 1 & pred < 0.01] = 0
table(labels2)


feature.names <- names(train)
feature.names <- feature.names[-match(c('ID', 'TARGET'), feature.names)]
feature.formula <- formula(paste('TARGET ~ ', paste(feature.names, collapse = ' + '), sep = ''))

dfull2 <- xgb.DMatrix(data.matrix(train[, feature.names]), label = labels2)

set.seed(300)
indexes <- sample(seq_len(nrow(train)), floor(nrow(train)*0.8))
dtrain2 <- xgb.DMatrix(data.matrix(train[indexes, feature.names]),
                      label = labels2[indexes])
dvalid2 <- xgb.DMatrix(data.matrix(train[-indexes, feature.names]),
                      label = labels2[-indexes])
watchlist <- list(valid = dvalid2, train = dtrain2)

set.seed(300)
model2 <- xgb.train(
  data = dtrain2,
  params = list(booster = "gbtree", objective = "binary:logistic",
                eval_metric = "auc", maximize = T,
                max_depth = 5,
                eta = 0.005,
                colsample_bytree = 0.5,
                subsample = 0.9),
  nrounds = 10000,
  early.stop.round = 100,
  watchlist = watchlist,
  print.every.n = 50
)

pred2 <- predict(model2, dfull)
# head(pred2, 20)
# table(pred2 > 0.1)

# Metrics::rmse(labels2, pred2)
# Metrics::auc(labels2, pred2)
# Metrics::auc(train$TARGET, pred2)

table(train[pred2 > 0.1, 'TARGET'])
table(train[pred2 > 0.05 & pred.full > 0.05, 'TARGET'])
table(train[pred2 > 0.2, 'TARGET'])

table(labels2[pred2 > 0.05])
View(train[train$TARGET == 1 & pred2 > 0.1, ])

table(labels2[pred2 > 0.1])
View(train[train$TARGET == 0 & pred2 > 0.1, ])

zzz <- data.frame(t = labels2, p = pred.full, p2 = pred2, d = pred.full/pred2)
View(zzz[(zzz$t == 1 | zzz$p > 0.1), ])
View(zzz[(zzz$t == 1 | zzz$p2 > 0.1) & zzz$d < 0.7, ])
View(zzz[(zzz$t == 1 | zzz$p2 > 0.1) & (zzz$t == 1 | zzz$d < 0.7 | zzz$d > 1.3), ])



pred_mixed <- pred.full
# indexes <- pred > 0.01 & pred < 0.1
# indexes <- pred.full > 0.1
indexes <- pred2 > 0.2 & pred.full > 0.2 & (pred.full/pred2 < 0.7)
# table(indexes)
pred_mixed[indexes] <- pred2[indexes]
# pred_mixed[indexes] <- 1
# pred_mixed[indexes] <- 0.35

Metrics::rmse(train$TARGET, pred_mixed)
Metrics::auc(train$TARGET, pred_mixed)




pred2.test <- predict(model2, dtest)
# table(pred2.test > 0.1)

pred_mixed <- partly_predict_09_11_27_2200_0.1_1_0.7_0.8_0.2$TARGET
# indexes <- partly_predict_13_27_30_2200_0.1_1_0.9_0.1$TARGET > 0.01 & partly_predict_13_27_30_2200_0.1_1_0.9_0.1$TARGET < 0.1
# indexes <- partly_predict_09_11_27_2200_0.1_1_0.7_0.8_0.2$TARGET >  0.1 & pred2.test > 0.1 &
#   (partly_predict_09_11_27_2200_0.1_1_0.7_0.8_0.2$TARGET / pred2.test < 0.7)
indexes <- pred2.test > 0.1 &
  (partly_predict_09_11_27_2200_0.1_1_0.7_0.8_0.2$TARGET / pred2.test < 0.7)
table(indexes)
pred_mixed[indexes] <- pred_mixed[indexes]*0.8 + pred2.test[indexes]*0.2
pred_mixed[indexes] <- pred2.test[indexes]
# pred_mixed[indexes] <- 0.35


zzz <- data.frame(p = partly_predict_09_11_27_2200_0.1_1_0.7_0.8_0.2$TARGET, p2 = pred2.test, pm = pred_mixed)
View(zzz)
View(zzz[zzz$p > 0.1, ])
View(zzz[c(3453, 3994, 15412, 19540, 23430, 34705, 62577, 67370, 68658), ])
View(zzz[c(166, 234, 979, 1758, 2256), ])

submission <- data.frame(ID = test$ID, TARGET = pred_mixed)
submissionName <- paste("./results/partly_predict",
                        format(Sys.time(), "%H_%M_%S"),
                        2500, 0.1, 1, 0.7, 0.8, 0.2,
                        sep = '_')
submissionFile <- paste0(submissionName, ".csv")
write.csv(submission, submissionFile, row.names=FALSE, quote = FALSE)






# ====================================================================================
pred_mixed <- xggb_163250_4_4$TARGET
indexes <- xggb_163250_4_4$TARGET > 0.1 & pred2.test > 0.1 &
  (xggb_163250_4_4$TARGET / pred2.test < 0.5)
table(indexes)
pred_mixed[indexes] <- pred_mixed[indexes]*0.1 + pred2.test[indexes]*0.9
# pred_mixed[indexes] <- pred2.test[indexes]
# pred_mixed[indexes] <- 0.35

zzz <- data.frame(p = xggb_163250_4_4$TARGET, p2 = pred2.test, pm = pred_mixed)
View(zzz[indexes, ])

submission <- data.frame(ID = test$ID, TARGET = pred_mixed)
submissionName <- paste("./igor/pp",
                        format(Sys.time(), "%H%M%S"),
                        0.5, 0.1, 0.9,
                        sep = '_')
submissionFile <- paste0(submissionName, ".csv")
write.csv(submission, submissionFile, row.names=FALSE, quote = FALSE)











# ====================================================================================
feature.names <- names(train)
feature.names <- feature.names[-match(c('ID', 'TARGET'), feature.names)]


labels2 <- train$TARGET
labels2[train$TARGET == 1 & pred > 0.1] = 0
labels2[train$TARGET == 1 & pred < 0.01] = 0
table(labels2)

labels2 <- train$TARGET
labels2[train$TARGET == 1 & (pred < 0.1 | pred > 0.2)] = 0
table(labels2)

dfull2 <- xgb.DMatrix(data.matrix(train[, feature.names]), label = labels2)

xgb_params <- list(booster = "gbtree", objective = "binary:logistic",
                   eval_metric = "auc",
                   # scale_pos_weight = 0.1,
                   max_depth = 5,
                   eta = 0.01,
                   colsample_bytree = 0.5,
                   subsample = 0.7)

for (j in 1:5) {
  seed.number <- 100000 + j
  set.seed(seed.number)
  
  xgb_cv = xgb.cv(params = xgb_params,
                  data = dfull2,
                  nrounds = 5000,
                  nfold = 5,
                  prediction = F,
                  showsd = F,
                  stratified = F,
                  verbose = T,
                  print.every.n = 100,
                  early.stop.round = 100)
  
  print(max(xgb_cv$test.auc.mean))
  print(which.max(xgb_cv$test.auc.mean))
}


set.seed(100)
model2 <- xgb.train(
  data = dfull2,
  params = xgb_params,
  nrounds = 2200)

pred2 <- predict(model2, dfull)
# head(pred2, 20)

Metrics::rmse(labels2, pred2)
Metrics::auc(labels2, pred2)


pred_mixed <- pred
indexes <- pred > 0.1 & pred < 0.2
pred_mixed[indexes] <- pred2[indexes]
# pred_mixed[indexes] <- pred_mixed[indexes]*0.99 + pred2[indexes]*0.01

Metrics::rmse(train$TARGET, pred_mixed)
Metrics::auc(train$TARGET, pred_mixed)
View(data.frame(t = train[indexes, 'TARGET'],
                p_o = pred[indexes],
                p_p = pred_mixed[indexes],
                pred[indexes] < pred_mixed[indexes]
))




pred2.test <- predict(model2, dtest)

pred_mixed <- partly_predict_00_01_10_0.01_0.1_0.9_0.1$TARGET
indexes <- partly_predict_00_01_10_0.01_0.1_0.9_0.1$TARGET > 0.1 & partly_predict_00_01_10_0.01_0.1_0.9_0.1$TARGET < 0.2
# table(indexes)
pred_mixed[indexes] <- pred_mixed[indexes]*0.99 + pred2.test[indexes]*0.01
# View(data.frame(p_o = pred_mixed[indexes], p_p = pred2.test[indexes]))

save.submission(test$ID, pred_mixed, 'results', 'partly_predict_round2', c(2200, 0.1, 0.2, 0.99, 0.01))

