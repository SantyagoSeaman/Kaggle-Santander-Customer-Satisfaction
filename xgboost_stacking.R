# -------------------------------------------------------------------------------------------------
# results.stats <- results.stats[-1, ]
mostAccurateDatasetsIndexes <- which(results.stats$rmse_winner < 0.11 & results.stats$auc_valid > 0.845)

resultsTrainDataFrame <- data.frame(results.train[mostAccurateDatasetsIndexes])
names(resultsTrainDataFrame) <- paste('V', 1:ncol(resultsTrainDataFrame), sep = '_')
resultsTrainDataFrame$TARGET <- train$TARGET

resultsTestDataFrame <- data.frame(results.test[mostAccurateDatasetsIndexes])
names(resultsTestDataFrame) <- paste('V', 1:ncol(resultsTestDataFrame), sep = '_')
resultsTestDataFrame$TARGET <- -1



# -------------------------------------------------------------------------------------------------
metrics.h2o.random <- readRDS('metricsListXgb_random.rds')
metrics.h2o.random <- data.frame(t(data.frame(metrics.h2o.random)))
names(metrics.h2o.random) <- paste('h2o', 1:ncol(metrics.h2o.random), sep = '_')
row.names(metrics.h2o.random) <- NULL
results.test.h2o.random <- readRDS('predictionsListXgb_random.rds')
results.test.h2o.random <- data.frame(results.test.h2o.random)
names(results.test.h2o.random) <- paste('h2o', 1:ncol(results.test.h2o.random), sep = '_')
results.train.h2o.random <- readRDS('predictionsListXgbTrain_random.rds')
results.train.h2o.random <- data.frame(results.train.h2o.random)
names(results.train.h2o.random) <- paste('h2o', 1:ncol(results.train.h2o.random), sep = '_')

mostAccurateH2ODatasetsIndexes <- which(metrics.h2o.random$h2o_8 < 0.02 & metrics.h2o.random$h2o_5 > 0.84)

resultsTrainH2ODataFrame <- data.frame(results.train.h2o.random[mostAccurateH2ODatasetsIndexes])
names(resultsTrainH2ODataFrame) <- paste('h2o', 1:ncol(resultsTrainH2ODataFrame), sep = '_')

resultsTestH2ODataFrame <- data.frame(results.test.h2o.random[mostAccurateH2ODatasetsIndexes])
names(resultsTestH2ODataFrame) <- paste('h2o', 1:ncol(resultsTestH2ODataFrame), sep = '_')


resultsTrainDataFrame <- cbind(resultsTrainDataFrame, resultsTrainH2ODataFrame)
resultsTestDataFrame <- cbind(resultsTestDataFrame, resultsTestH2ODataFrame)


# -------------------------------------------------------------------------------------------------
feature.names.stacking <- names(resultsTrainDataFrame)
feature.names.stacking <- feature.names.stacking[-match(c('TARGET'), feature.names.stacking)]
feature.formula.stacking <- formula(paste('TARGET ~ ', paste(feature.names.stacking, collapse = ' + '), sep = ''))


seed.number = 10
set.seed(seed.number)
indexes <- sample(seq_len(nrow(resultsTrainDataFrame)), floor(nrow(resultsTrainDataFrame)*0.5))
dtrain <- xgb.DMatrix(data.matrix(resultsTrainDataFrame[indexes, feature.names.stacking]),
                      label = resultsTrainDataFrame[indexes, 'TARGET'])
dvalid <- xgb.DMatrix(data.matrix(resultsTrainDataFrame[-indexes, feature.names.stacking]),
                      label = resultsTrainDataFrame[-indexes, 'TARGET'])
dfull <- xgb.DMatrix(data.matrix(resultsTrainDataFrame[, feature.names.stacking]),
                     label = resultsTrainDataFrame[, 'TARGET'])
dtest <- data.matrix(resultsTestDataFrame[, feature.names.stacking])

watchlist <- list(eval = dvalid, train = dtrain)
params <- list(booster = "gbtree", objective = "binary:logistic",
               eval_metric = 'rmse', maximize = F,
               max_depth = 6, eta = 0.05,
               colsample_bytree = 0.7, subsample = 1)
# params <- list(booster = "gblinear", objective = "binary:logistic",
#                eval_metric = 'rmse', maximize = F,
#                # gamma = 10,
#                eta = 0.05)

xgb_cv = xgb.cv(params = params,
                data = dfull,
                nrounds = 2001,
                nfold = 3,
                prediction = F,
                showsd = F,
                stratified = F,
                verbose = T,
                print.every.n = 10,
                early.stop.round = 20)

min.logloss <- max(xgb_cv$test.logloss.mean)
min.logloss.index <- which.min(xgb_cv$test.logloss.mean)

for(i in 1:1000) {
  seed.number = i

  set.seed(seed.number)
  indexes <- sample(seq_len(nrow(resultsTrainDataFrame)), floor(nrow(resultsTrainDataFrame)*0.5))
  dtrain <- xgb.DMatrix(data.matrix(resultsTrainDataFrame[indexes, feature.names.stacking]),
                        label = resultsTrainDataFrame[indexes, 'TARGET'])
  dvalid <- xgb.DMatrix(data.matrix(resultsTrainDataFrame[-indexes, feature.names.stacking]),
                        label = resultsTrainDataFrame[-indexes, 'TARGET'])


  nr <- round(runif(1, 1, 100), 0)

  set.seed(seed.number)
  watchlist <- list(eval = dvalid, train = dtrain)
  model <- xgb.train(params = params, data = dtrain,
                     nrounds = 500, early.stop.round = 20,
                     watchlist = watchlist, print.every.n = 10)

  pred.train <- predict(model, dfull)
  # head(pred.train, 20)
  # head(train$TARGET, 20)
  train.rmse <- rmse(train$TARGET, pred.train)
  train.logloss <- logLoss(train$TARGET, pred.train)
  
  pred.test <- predict(model, dtest)
  # round(head(pred.test, 50), 5)
  # round(head(partly_predict_09_11_27_2200_0.1_1_0.7_0.8_0.2$TARGET, 50), 5)
  test.rmse <- rmse(partly_predict_13_47_05_2500_0.1_1_0.7_0.8_0.2$TARGET, pred.test)
  test.logloss <- logLoss(partly_predict_13_47_05_2500_0.1_1_0.7_0.8_0.2$TARGET, pred.test)
  
  cat('\nRounds: ', nr, ', train RMSE: ', train.rmse, ', test RMSE: ', test.rmse, '\n', sep = '')
  cat('Train logloss: ', train.logloss, ', test logloss: ', test.logloss, '\n', sep = '')
  cat("=========================================================================================\n")

  #   model <- glm(feature.formula.stacking, data = resultsTrainDataFrame[indexes, ], family = binomial())
  #   pred.train <- predict(model, resultsTrainDataFrame, type='response')
  # #   head(pred, 10)
  # #   head(resultsTrainDataFrame[-indexes, 'target'], 10)
  #   rmse(resultsTrainDataFrame$target, pred.train)
  #   pred.test <- predict(model, resultsTestDataFrame, type='response')
  #   rmse(h2o_xgb_100840_ensemble_0.8_0.1_0.1$PredictedProb, pred.test)
}

partly_predict_13_47_05_2500_0.1_1_0.7_0.8_0.2$pred <- pred.test

save.submission(test$ID, pred.test, 'results', 'xgboost', c('stacking', 'gblinear', 100))
save.submission(test$ID, pred.test, 'results', 'xgboost', c('stacking', 'gbtree', 181))

save.submission(test$ID, pred.test, 'igor', 'xgs', c('linear', 1000))

save.submission(test$ID, partly_predict_13_47_05_2500_0.1_1_0.7_0.8_0.2$TARGET*0.9 + pred.test*0.1,
                'results', 'ensemble', c('partly.winner', 'stacking', 0.9, 0.1))


results.mean <- rowMeans(resultsTestDataFrame[, feature.names.stacking])
save.submission(test$ID, results.mean, 'results', 'xgboost_h2o', c('mean'))



