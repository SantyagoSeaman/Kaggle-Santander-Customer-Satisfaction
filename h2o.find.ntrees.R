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

for (trees in seq(1500, 2000, 100)) {
  print("************************************************************************************************")
  print(paste0("Trees: ", trees))

  for(i in 1:5) {
    print("************************************************************************************************")
    print(paste0("Round: ", i))

    set.seed(i)
    indexes <- sample(seq_len(nrow(train)), floor(nrow(train)*0.8))
    trainHex <- as.h2o(train[indexes, c(feature.names, 'TARGET')], destination_frame="train.hex")
    #summary(trainHex)
    validHex <- as.h2o(train[-indexes, c(feature.names, 'TARGET')], destination_frame="valid.hex")

#     print("*******  RANDOM FOREST *******")
#     rfHex <- h2o.randomForest(x = feature.names,, y = "TARGET",
#                               model_id = "rfStarter.hex",
#                               training_frame = trainHex,
#                               ntrees = trees,
#                               mtries = ceiling(ncol(train)/2),
#                               sample_rate = 0.7,
#                               max_depth = 5,
#                               # nfolds = 5,
#                               validation_frame = validHex,
#                               stopping_rounds = 20,
#                               stopping_metric = 'AUC',
#                               stopping_tolerance = 0.0001,
#                               balance_classes = T,
#                               # keep_cross_validation_predictions = T,
#                               seed = 123 + i)
#     print(rfHex@model$model_summary)
#     print(rfHex@model$validation_metrics)
#     
#     metrics <- c(auc(train[-indexes, 'TARGET'], as.data.frame(h2o.predict(rfHex, validHex))$p1),
#                  auc(train[indexes, 'TARGET'], as.data.frame(h2o.predict(rfHex, trainHex))$p1),
#                  auc(train$TARGET, as.data.frame(h2o.predict(rfHex, fullHex))$p1))
#     
#     pred <- as.data.frame(h2o.predict(rfHex, testHex))$p1
#     predictionsListRf <- c(predictionsListRf, list(pred))
#     metrics <- c(metrics, Metrics::rmse(partly_predict_00_01_10_0.01_0.1_0.9_0.1$TARGET, pred))
#     
#     print(paste("Valid AUC", round(metrics[1], 7),
#                 ", Train AUC", round(metrics[2], 7),
#                 ", Full AUC", round(metrics[3], 7),
#                 ", Winner RMSE", round(metrics[4], 7)))
#     metricsListRf <- c(metricsListRf, list(metrics))
#     
#     saveRDS(predictionsListRf, 'predictionsListRf2.rds')
#     saveRDS(metricsListRf, 'metricsListRf2.rds')


    print("*******  GRADIENT BOOSTING *******")
    gbmHex <- h2o.gbm(x = feature.names,
                      y = "TARGET", model_id="gbmStarter.hex",
                      training_frame = trainHex,
                      # distribution="AUTO",
                      ntrees = trees,
                      learn_rate = 0.005,
                      max_depth = 5,
                      col_sample_rate = 0.3,
                      sample_rate = 0.7,
                      validation_frame = validHex,
                      # min_rows = 10,
                      stopping_rounds = 20,
                      stopping_metric = 'AUC',
                      stopping_tolerance = 0.0001,
                      balance_classes = T,
                      seed = 123 + i)
    print(gbmHex@model$model_summary)
    print(gbmHex@model$validation_metrics)

    metrics <- c(auc(train[-indexes, 'TARGET'], as.data.frame(h2o.predict(gbmHex, validHex))$p1),
                 auc(train[indexes, 'TARGET'], as.data.frame(h2o.predict(gbmHex, trainHex))$p1),
                 auc(train$TARGET, as.data.frame(h2o.predict(gbmHex, fullHex))$p1))

    # summary(gbmHex)
    # head(gbmHex@model$variable_importances, 20)
    pred <- as.data.frame(h2o.predict(gbmHex, testHex))$p1
    predictionsListXgb <- c(predictionsListXgb, list(pred))
    metrics <- c(metrics, Metrics::rmse(partly_predict_09_11_27_2200_0.1_1_0.7_0.8_0.2$TARGET, pred))

    print(paste("Valid AUC", round(metrics[1], 7),
                ", Train AUC", round(metrics[2], 7),
                ", Full AUC", round(metrics[3], 7),
                ", Winner RMSE", round(metrics[4], 7)))
    metricsListXgb <- c(metricsListXgb, list(metrics))

    saveRDS(predictionsListXgb, 'predictionsListXgb_1500_2000_6.rds')
    saveRDS(metricsListXgb, 'metricsListXgb_1500_2000_6.rds')

    #   pred <- as.data.frame(h2o.predict(gbmHex, validHex))
    #   pred <- as.data.frame(h2o.predict(gbmHex, testHex))[3]
  }
}



# predictionsListXgb <- readRDS('winner_models/predictionsListXgb.rds')







results.mean.final <- (predictionsListXgb[[7]] + predictionsListXgb[[8]] + predictionsListXgb[[9]])/3
results.mean.final <- (predictionsListXgb[[7]] + predictionsListXgb[[8]] + predictionsListXgb[[9]] + predictionsListXgb[[10]] + predictionsListXgb[[11]] + predictionsListXgb[[12]])/6
rmse.winner.cummulative <- Metrics::rmse(partly_predict_00_01_10_0.01_0.1_0.9_0.1$TARGET, results.mean.final)

results.mean.final <- partly_predict_09_11_27_2200_0.1_1_0.7_0.8_0.2$TARGET*0.99 + predictionsListXgb[[9]]*0.01
results.mean.final <- predictionsListXgb[[3]]

save.submission(test$ID, results.mean.final, 'results', 'h2o_gbm',
                c(feature.names.length, 0.005, 5, 0.5, 0.7, trees, metricsListXgb[[3]]))
