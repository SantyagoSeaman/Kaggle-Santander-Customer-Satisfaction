# Using 150 features
# LB 0.841344

library(h2o)
# library(data.table)
library(Metrics)

h2o.init(nthreads=-1, min_mem_size = "4G", max_mem_size = "12G")
# h2o.shutdown(F)

feature.names <- names(train)
feature.names <- feature.names[-match(c('ID', 'TARGET'), feature.names)]
feature.names <- feature.names[-grep('delta', feature.names)]
feature.formula <- formula(paste('TARGET ~ ', paste(feature.names, collapse = ' + '), sep = ''))

train$TARGET <- factor(train$TARGET)
indexes <- sample(seq_len(nrow(train)), floor(nrow(train)*0.8))
trainHex <- as.h2o(train[indexes, c(feature.names, 'TARGET')], destination_frame="train.hex")
validHex <- as.h2o(train[-indexes, c(feature.names, 'TARGET')], destination_frame="valid.hex")
testHex <- as.h2o(test[, feature.names], destination_frame="test.hex")
fullHex <- as.h2o(train[, c(feature.names, 'TARGET')], destination_frame="full.hex")

dpHex = h2o.deeplearning(x = feature.names,
                         y = "TARGET",
                         training_frame = trainHex,
                         validation_frame = validHex,
                         model_id = "dlStarter.hex",
                         activation = "Tanh",
                         hidden = c(100, 100, 100, 100, 100),
                         epochs = 2000,
                         stopping_rounds = 50,
                         stopping_metric = "AUC",
                         balance_classes = T,
                         seed = 3)
pred <- as.data.frame(h2o.predict(dpHex, testHex))
xg_grid_cv_bag_09_38_0247$pred2 <- pred$p1
Metrics::rmse(xg_grid_cv_bag_09_38_0247$TARGET, xg_grid_cv_bag_09_38_0247$pred2)

pred <- as.data.frame(h2o.predict(dpHex, fullHex))
Metrics::auc(train[, 'TARGET'], pred$p1)
Metrics::rmse(as.integer(as.character(train[, 'TARGET'])), pred$p1)

pred <- xg_grid_cv_bag_09_38_0247$TARGET*0.8 + h2o_deeplearning_20_07_49$TARGET*0.2

submission <- data.frame(ID = test$ID, TARGET = pred)
submissionName <- paste("./results/h2o_deeplearning_bag_winner841287",
                        format(Sys.time(), "%H_%M_%S"),
                        0.8, 0.2, 2000, 100, 100, 100, 100, 100,
                        sep = '_')
submissionFile <- paste0(submissionName, ".csv")
write.csv(submission, submissionFile, row.names=FALSE, quote = FALSE)

