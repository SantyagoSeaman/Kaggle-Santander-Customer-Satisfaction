library(h2o)
# library(data.table)
library(Metrics)

# rm(orig.train)
# rm(orig.test)
# rm(merged)
# gc()

h2o.init(nthreads=-1, min_mem_size = "4G", max_mem_size = "10G")
# h2o.shutdown(F)


feature.names <- names(train)
feature.names <- feature.names[-match(c('ID', 'TARGET'), feature.names)]
feature.formula <- formula(paste('TARGET ~ ', paste(feature.names, collapse = ' + '), sep = ''))

train$TARGET <- factor(train$TARGET)
testHex <- as.h2o(test[, feature.names], destination_frame="test.hex")
fullHex <- as.h2o(train[, c(feature.names, 'TARGET')], destination_frame="full.hex")

predictionsListRf <- list()
predictionsListXgb <- list()
metricsListRf <- list()
metricsListXgb <- list()
predictionsListXgbTrain <- list()

for(i in 1:1000) {
  cat("*** Step: ", i, " ******************************************************************************************************************************\n")

  set.seed(i)
  indexes <- sample(seq_len(nrow(train)), floor(nrow(train)*0.8))
  trainHex <- as.h2o(train[indexes, c(feature.names, 'TARGET')], destination_frame="train.hex")
  #summary(trainHex)
  validHex <- as.h2o(train[-indexes, c(feature.names, 'TARGET')], destination_frame="valid.hex")

  # lr = runif(1, 0.005, 0.01)
  trees = round(runif(1, 500, 3000), 0)
  md = round(runif(1, 4, 10), 0)
  csr = round(runif(1, 0.2, 0.7), 2)
  sr = round(runif(1, 0.6, 0.9), 2)

  gbmHex <- h2o.gbm(x = feature.names,
                    y = "TARGET", model_id="gbmStarter.hex",
                    training_frame = trainHex,
                    ntrees = trees,
                    learn_rate = 0.005,
                    max_depth = md,
                    col_sample_rate = csr,
                    sample_rate = sr,
                    validation_frame = validHex,
                    # min_rows = 10,
                    stopping_rounds = 20,
                    stopping_metric = 'AUC',
                    stopping_tolerance = 0.0001,
                    balance_classes = T,
                    seed = 123 + i)
  print(gbmHex@model$model_summary)
  print(gbmHex@model$validation_metrics)
  
  metrics <- c(trees, md, csr, sr,
               auc(train[-indexes, 'TARGET'], as.data.frame(h2o.predict(gbmHex, validHex))$p1),
               auc(train[indexes, 'TARGET'], as.data.frame(h2o.predict(gbmHex, trainHex))$p1),
               auc(train$TARGET, as.data.frame(h2o.predict(gbmHex, fullHex))$p1))

  # summary(gbmHex)
  # head(gbmHex@model$variable_importances, 20)
  pred <- as.data.frame(h2o.predict(gbmHex, testHex))$p1
  predictionsListXgb <- c(predictionsListXgb, list(pred))
  metrics <- c(metrics, Metrics::rmse(partly_predict_00_01_10_0.01_0.1_0.9_0.1$TARGET, pred))

  pred <- as.data.frame(h2o.predict(gbmHex, fullHex))$p1
  predictionsListXgbTrain <- c(predictionsListXgbTrain, list(pred))
  metrics <- c(metrics, Metrics::rmse(train$TARGET, pred))

  cat(
    "Trees ", metrics[1],
    ", max depth ", metrics[2],
    ", col sample rate ", metrics[3],
    ", sample rate ", metrics[4],
    ", Valid AUC ", round(metrics[5], 7),
    ", Train AUC ", round(metrics[6], 7),
    ", Full AUC ", round(metrics[7], 7),
    ", Winner RMSE ", round(metrics[8], 7),
    ", Train RMSE ", round(metrics[9], 7),
    "\n", sep = '')
  metricsListXgb <- c(metricsListXgb, list(metrics))
  
  saveRDS(predictionsListXgb, 'predictionsListXgb_random.rds')
  saveRDS(predictionsListXgbTrain, 'predictionsListXgbTrain_random.rds')
  saveRDS(metricsListXgb, 'metricsListXgb_random.rds')
}



# predictionsListXgb <- readRDS('winner_models/predictionsListXgb.rds')







results.mean.final <- (predictionsListXgb[[7]] + predictionsListXgb[[8]] + predictionsListXgb[[9]])/3
results.mean.final <- (predictionsListXgb[[7]] + predictionsListXgb[[8]] + predictionsListXgb[[9]] + predictionsListXgb[[10]] + predictionsListXgb[[11]] + predictionsListXgb[[12]])/6
rmse.winner.cummulative <- Metrics::rmse(partly_predict_00_01_10_0.01_0.1_0.9_0.1$TARGET, results.mean.final)

results.mean.final <- partly_predict_00_01_10_0.01_0.1_0.9_0.1$TARGET*0.99 + predictionsListXgb[[9]]*0.01
results.mean.final <- predictionsListXgb[[3]]

save.submission(test$ID, results.mean.final, 'results', 'h2o_gbm',
                c(feature.names.length, 0.005, 5, 0.5, 0.7, trees, metricsListXgb[[3]]))
