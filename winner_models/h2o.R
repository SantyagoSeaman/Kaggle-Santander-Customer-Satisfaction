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
# feature.names <- feature.names[grep('^var', feature.names)]
# feature.names <- feature.names[grep('saldo', feature.names)]
# feature.names <- feature.names[-grep('delta', feature.names)]
# feature.names <- feature.names[-grep('^imp_', feature.names)]
# feature.names <- feature.names[-grep('^ind_', feature.names)]
# feature.names <- feature.names[-grep('^num_', feature.names)]
# feature.names <- feature.names[-grep('d__.*?var30.*', feature.names)]
# feature.names <- feature.names[-grep('d__.*', feature.names)]
feature.formula <- formula(paste('TARGET ~ ', paste(feature.names, collapse = ' + '), sep = ''))

train$TARGET <- factor(train$TARGET)
testHex <- as.h2o(test[, feature.names], destination_frame="test.hex")

predictionsListRf <- list()
predictionsListXgb <- list()
i <- 1
for(i in 1:1000)
{
  print("************************************************************************************************")
  print("************************************************************************************************")
  print(paste0("Round: ", i))
  
  set.seed(i)
  indexes <- sample(seq_len(nrow(train)), floor(nrow(train)*0.8))
  trainHex <- as.h2o(train[indexes, c(feature.names, 'TARGET')], destination_frame="train.hex")
  #summary(trainHex)
  validHex <- as.h2o(train[-indexes, c(feature.names, 'TARGET')], destination_frame="valid.hex")
  fullHex <- as.h2o(train[, c(feature.names, 'TARGET')], destination_frame="full.hex")
  
  rfHex <- h2o.randomForest(x = feature.names, y = "TARGET",
                            model_id = "rfStarter.hex",
                            training_frame = trainHex,
                            ntrees = 5000,
                            sample_rate = 0.7,
                            mtries = 50,
                            max_depth = 6,
                            # nfolds = 5,
                            validation_frame = validHex,
                            stopping_rounds = 100,
                            stopping_metric = 'AUC',
                            stopping_tolerance = 1e-8,
                            balance_classes = T,
                            # keep_cross_validation_predictions = T,
                            seed = 123 + i)
  print(rfHex@model$validation_metrics)

  pred <- as.data.frame(h2o.predict(rfHex, testHex))$p1
  predictionsListRf <- c(predictionsListRf, list(pred))
  print(Metrics::rmse(xg_grid_cv_bag_09_38_0247$TARGET, pred))

  saveRDS(predictionsListRf, 'predictionsListRf.rds')

#   gbmHex <- h2o.gbm(x = feature.names,
#                     y = "TARGET", model_id="gbmStarter.hex",
#                     training_frame = fullHex,
#                     # distribution="AUTO",
#                     ntrees = 2500,
#                     learn_rate = 0.005,
#                     sample_rate = 0.7,
#                     col_sample_rate = 0.5,
#                     max_depth = 5,
#                     # validation_frame = validHex,
#                     nfolds = 7,
#                     keep_cross_validation_predictions = T,
#                     # min_rows = 10,
#                     stopping_rounds = 200,
#                     stopping_metric = 'AUC',
#                     stopping_tolerance = 1e-8,
#                     balance_classes = T,
#                     seed = 123 + i)

  gbmHex <- h2o.gbm(x = feature.names,
                    y = "TARGET", model_id="gbmStarter.hex",
                    training_frame = trainHex,
                    # distribution="AUTO",
                    ntrees = 1500,
                    learn_rate = 0.005,
                    sample_rate = 0.7,
                    col_sample_rate = 0.5,
                    max_depth = 5,
                    validation_frame = validHex,
                    # nfolds = 5,
                    # keep_cross_validation_predictions = T,
                    # min_rows = 10,
                    stopping_rounds = 100,
                    stopping_metric = 'AUC',
                    stopping_tolerance = 1e-8,
                    balance_classes = T,
                    seed = 123 + i)
  print(gbmHex@model$validation_metrics)
  # summary(gbmHex)
  # head(gbmHex@model$variable_importances, 20)
  pred <- as.data.frame(h2o.predict(gbmHex, testHex))$p1
  predictionsListXgb <- c(predictionsListXgb, list(pred))
  print(Metrics::rmse(xg_grid_cv_bag_09_38_0247$TARGET, pred))

  saveRDS(predictionsListXgb, 'predictionsListXgb.rds')

#   pred <- as.data.frame(h2o.predict(gbmHex, validHex))
#   pred <- as.data.frame(h2o.predict(gbmHex, testHex))[3]
}


predictionsListXgb <- readRDS('predictionsListXgb.rds')



results.mean <- as.data.frame(predictionsListXgb[[3]])
for(i in c(5,7,10,11,12,15,17)) {
  pred <- as.data.frame(predictionsListXgb[[i]])
  results.mean <- cbind(results.mean, pred)
}
results.mean <- rowMeans(results.mean)


xg_grid_cv_bag_09_38_0247$pred_1 <- results.mean

winner_pred <- pred$p1


Metrics::auc(train[-indexes, 'TARGET'], pred$p1)

Metrics::rmse(xg_grid_cv_bag_09_38_0247$TARGET, xg_grid_cv_bag_09_38_0247$pred_1)


h2o_deeplearning_bag_winner841287$winner_1 <- xg_grid_cv_bag_09_38_0247$TARGET
h2o_deeplearning_bag_winner841287$pred_1 <- results.mean
Metrics::rmse(h2o_deeplearning_bag_winner841287$TARGET, h2o_deeplearning_bag_winner841287$pred_1)


submission <- data.frame(ID = test$ID, TARGET = results.mean)
# submission <- data.frame(ID = test$ID, TARGET = pred)
submission <- data.frame(ID = test$ID, TARGET = pred$p1)
# submissionName <- paste0("./results/xg_bag_", format(Sys.time(), "%H_%M_%S"), '_rand_', length(results))
submissionName <- paste0("./results/h2o_gbm_", format(Sys.time(), "%H_%M_%S"), '_top_ensemble_deep_winner841287_02_08')
submissionName <- paste0("./results/h2o_gbm_", format(Sys.time(), "%H_%M_%S"))
submissionFile <- paste0(submissionName, ".csv")
write.csv(submission, submissionFile, row.names=FALSE, quote = FALSE)


submission$TARGET <- submission$TARGET*0.2 + h2o_deeplearning_bag_winner841287$TARGET*0.8

