library(h2o)
# library(data.table)
library(Metrics)

h2o.init(nthreads=-1, min_mem_size = "4G", max_mem_size = "10G")
# h2o.shutdown(F)

# rm(merged, orig.train, orig.test)
# gc()


feature.names <- names(train)
feature.names <- feature.names[-match(c('ID', 'TARGET'), feature.names)]
feature.formula <- formula(paste('TARGET ~ ', paste(feature.names, collapse = ' + '), sep = ''))

train$TARGET <- factor(train$TARGET)
indexes <- sample(seq_len(nrow(train)), floor(nrow(train)*0.8))
trainHex <- as.h2o(train[indexes, c(feature.names, 'TARGET')], destination_frame="train.hex")
#summary(trainHex)
validHex <- as.h2o(train[-indexes, c(feature.names, 'TARGET')], destination_frame="valid.hex")
testHex <- as.h2o(test[, feature.names], destination_frame="test.hex")
fullHex <- as.h2o(train[, c(feature.names, 'TARGET')], destination_frame="full.hex")

predictionsList <- list()
predictionsListFull <- list()
for(i in 1:100) {
  print("=======================================================================================")
#   rfHex <- h2o.randomForest(x = feature.names, y = "TARGET", training_frame = trainHex, model_id = "rfStarter.hex",
#                             ntrees = 20,
#                             sample_rate = 0.7,
#                             mtries = 5,
#                             nfolds = 7,
#                             stopping_rounds = 100,
#                             stopping_metric = 'AUC',
#                             balance_classes = F,
#                             keep_cross_validation_predictions = T)
#   summary(rfHex)
#   predictionsList <- c(predictionsList, h2o.predict(rfHex, testHex))

  gbmHex <- h2o.gbm(x = feature.names,
                    y = "TARGET",
                    training_frame = fullHex,
                    model_id = "gbmStarter.hex",
                    distribution = "AUTO",
#                     nfolds = 7,
#                     stopping_rounds = 20,
#                     stopping_metric = 'AUC',
#                     stopping_tolerance = 0.0001,
                    seed = 123 + i,
                    ntrees = 10000,
                    max_depth = 7,
                    # min_rows = 10,
                    learn_rate = 0.005,
                    col_sample_rate = 0.5,
                    sample_rate = 0.7,
                    keep_cross_validation_predictions = F,
                    balance_classes = T)
  print(gbmHex)
  print(summary(gbmHex))
  # head(gbmHex@model$variable_importances, 20)
  predictionsList <- c(predictionsList, h2o.predict(gbmHex, testHex))
  predictionsListFull <- c(predictionsListFull, h2o.predict(gbmHex, fullHex))
  
#   pred <- as.data.frame(h2o.predict(gbmHex, validHex))
#   pred <- as.data.frame(h2o.predict(gbmHex, testHex))[3]
}


dpHex = h2o.deeplearning(x = feature.names,
                         y = "TARGET",
                         training_frame = fullHex,
                         # validation_frame = validHex,
                         nfolds = 7,
                         model_id = "dlStarter.hex",
                         activation = "Tanh",
                         hidden = c(100),
                         epochs = 100,
#                          stopping_rounds = 20,
#                          stopping_metric = "AUC",
#                          stopping_tolerance = 0.0001,
#                          l1 = 1e-5,
#                          l2 = 1e-5,
                         # reproducible = T,
                         balance_classes = T,
                         seed = 5)
pred <- as.data.frame(h2o.predict(dpHex, testHex))
# xg_grid_cv_bag_09_38_0247$pred2 <- pred$p1
# Metrics::rmse(xg_grid_cv_bag_09_38_0247$TARGET, xg_grid_cv_bag_09_38_0247$pred2)

pred <- as.data.frame(h2o.predict(dpHex, fullHex))$p1
Metrics::auc(train[, 'TARGET'], pred)
Metrics::rmse(as.integer(as.character(train[, 'TARGET'])), pred)
zzz <- data.frame(t = as.integer(as.character(train[, 'TARGET'])), p = pred)

Metrics::auc(train[-indexes, 'TARGET'], as.data.frame(h2o.predict(dpHex, validHex))$p1)


pred.list$p2 <- pred$p1




pred <- h2o_deeplearning_20_07_49$TARGET*0.2 + xg_grid_cv_bag_09_38_0247$TARGET*0.8

submission$TARGET <- submission$TARGET*0.1 + xg_grid_cv_bag_09_38_0247$TARGET*0.9

# submission <- data.frame(ID = test$ID, TARGET = results.mean)
submission <- data.frame(ID = test$ID, TARGET = pred$p1)
# submissionName <- paste("./results/h2o_deeplearning", format(Sys.time(), "%H_%M_%S"), 1000, 30, 10, sep='_')
# submissionName <- paste0("./results/h2o_deeplearning_bag_", format(Sys.time(), "%H_%M_%S"))
submissionName <- paste("./results/h2o_deeplearning_bag_winner841287",
                        format(Sys.time(), "%H_%M_%S"),
                        0.1, 0.9, 1000, 30, 10,
                        sep = '_')
submissionFile <- paste0(submissionName, ".csv")
write.csv(submission, submissionFile, row.names=FALSE, quote = FALSE)


submission$Expected <- 0.7*predictions$rfv3cn3 + 0.1*predictions$pred1_inch + 0.1*predictions$pred2_inch + 0.1*predictions$pred3_inch
submission$Expected <- 0.79*means + 0.21 * submission$Expected
write.csv(submission, "rf_means.csv", row.names=F)

