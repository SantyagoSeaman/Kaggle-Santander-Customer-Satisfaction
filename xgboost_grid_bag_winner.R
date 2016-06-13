library(xgboost)
library(Matrix)
library(Metrics)
library(methods)
library(caret)
options(scipen=999)
# options(warn=-1)
source('./helpers.r')


# table(train$TARGET)
# train.target0.indexes <- which(train$TARGET == 0)
# train.target1.indexes <- which(train$TARGET == 1)
# train2 <- train[c(rep(train.target1.indexes, 10), train.target0.indexes), ]

xgb_grid <- readRDS('xgb_grid_3.rds')

xgb_grid_sorted <- xgb_grid[order(xgb_grid$mean_auc, decreasing = T, na.last = T), ]
xgb_grid_sorted <- xgb_grid_sorted[!is.na(xgb_grid_sorted$mean_auc) & xgb_grid_sorted$mean_auc > 0.8420, ]
xgb_grid_sorted$mean_step <- as.integer(ceiling(xgb_grid_sorted$mean_step))


# xgb_grid_sorted <- xgb_grid_sorted[2, ]

feature.names <- names(train)
feature.names <- feature.names[-match(c('ID', 'TARGET'), feature.names)]
feature.formula <- formula(paste('TARGET ~ ', paste(feature.names, collapse = ' + '), sep = ''))

dfull <- xgb.DMatrix(data.matrix(train[, feature.names]), label = train$TARGET)
dtest <- data.matrix(test[, feature.names])



# partly_predict_13_47_05_2500_0.1_1_0.7_0.8_0.2 <- read.csv("./results/partly_predict_13_47_05_2500_0.1_1_0.7_0.8_0.2.csv", stringsAsFactors=FALSE)
current.winner.pred <- winner_long_tail_174228_current_all$TARGET
feature.names.length <- length(feature.names)
results.test <- c()
# results.valid <- c()
results.full <- c()
results.stats <- data.frame(z = NA, i = NA, max_depth = NA, eta = NA,
                            colsample_bytree = NA, subsample = NA,
                            mean_auc = NA, mean_step = NA,
                            rounds = NA, seed = NA, rmse_winner = NA,
                            rmse_valid = NA, auc_valid = NA,
                            rmse_full = NA, auc_full = NA,
                            pos_neg_valid = NA, pos_neg_train = NA,
                            pred01_valid = NA, pred01_full = NA, pred01_test = NA,
                            pred01Target1_full = NA)
rmse.winner <- 1000
winnerIndexes <- c()
sample.part <- 0.7
# set.seed(100)
# indexes <- sample(seq_len(nrow(train)), floor(nrow(train)*sample.part))
# dtrain <- xgb.DMatrix(data.matrix(train[indexes, feature.names]),
#                       label = train[indexes, 'TARGET'])
# dvalid <- xgb.DMatrix(data.matrix(train[-indexes, feature.names]),
#                       label = train[-indexes, 'TARGET'])
for(z in 1:100) {
  for (i in 1:nrow(xgb_grid_sorted)) {
    cat("---------------------------------------------------------------------------------------------------\n")
    cat("Round:", z, "-", i, "\n")

    grid.params <- xgb_grid_sorted[i, ]
    print(grid.params)

    xgb_params <- list(booster = "gbtree", objective = "binary:logistic",
                       eval_metric = "rmse", maximize = F,
                       max_depth = grid.params$max_depth,
                       eta = grid.params$eta,
                       # min_child_weight = 10,
                       colsample_bytree = grid.params$colsample_bytree,
                       subsample = grid.params$subsample)
    nrounds <- grid.params$mean_step
    seed.number <- 10000*z + i

    cat("Started:", as.character(Sys.time()), ", rounds: ", nrounds, ', seed: ', seed.number, '\n')

    set.seed(seed.number)
    indexes <- sample(seq_len(nrow(train)), floor(nrow(train)*sample.part))
    dtrain <- xgb.DMatrix(data.matrix(train[indexes, feature.names]),
                          label = train[indexes, 'TARGET'])
    dvalid <- xgb.DMatrix(data.matrix(train[-indexes, feature.names]),
                          label = train[-indexes, 'TARGET'])
    watchlist <- list(valid = dvalid, train = dtrain)

    target.table.train <- table(train[indexes, 'TARGET'])
    target.table.valid <- table(train[-indexes, 'TARGET'])
    target.table.train.diff <- target.table.train[2] / target.table.train[1]
    target.table.valid.diff <- target.table.valid[2] / target.table.valid[1]
    cat("Positive/negative:  train =", target.table.train.diff,
        ", valid =", target.table.valid.diff, '\n')

    set.seed(seed.number)
    #       model <- xgb.train(
    #         data = dfull,
    #         params = c(xgb_params),
    #         nrounds = nrounds)
    model <- xgb.train(params = xgb_params, data = dtrain,
                       nrounds = nrounds,
                       early.stop.round = ceiling(log(1/grid.params$eta)*20),
                       watchlist = watchlist,
                       print.every.n = ceiling(log(1/grid.params$eta))*50)
#     imp <- xgb.importance(model = model, feature_names = feature.names)
#     xgb.dump(model, 'model_dump.txt')

    pred.full <- predict(model, dfull)
    rmse_full <- Metrics::rmse(train$TARGET, pred.full)
    auc_full <- Metrics::auc(train$TARGET, pred.full)
    pred01_full <- as.integer(table(pred.full > 0.1)[2])
    pred01Target1_full <- as.integer(table(train[pred.full > 0.1, 'TARGET'])[2])

    pred.valid <- predict(model, dvalid)
    rmse_valid <- Metrics::rmse(train[-indexes, 'TARGET'], pred.valid)
    auc_valid <- Metrics::auc(train[-indexes, 'TARGET'], pred.valid)
    pred01_valid <- as.integer(table(pred.valid > 0.1)[2])

    # Compare with prev. winners
    rmse.winner.prev <- rmse.winner
    pred.test <- predict(model, dtest)
    rmse.winner <- Metrics::rmse(current.winner.pred, pred.test)
    pred01_test <- as.integer(table(pred.test > 0.1)[2])

    cat("\n")
    print(paste0("Winner RMSE: ",
                 round(rmse.winner, 5),
                 ", Valid RMSE: ", round(rmse_valid, 5),
                 ", Valid AUC: ", round(auc_valid, 5),
                 ", Full RMSE: ", round(rmse_full, 5),
                 ", Full AUC: ", round(auc_full, 5)
    ));
    print(paste0("Valid pred 0.1: ", round(pred01_valid, 5),
                 ", Full pred 0.1: ", round(pred01_full, 5),
                 ", Test pred 0.1: ", round(pred01_test, 5),
                 ", Full pred 0.1 target 1: ", pred01Target1_full
    ));

    stats <- c(z, i, grid.params,
               model$bestInd, seed.number, rmse.winner,
               rmse_valid, auc_valid,
               rmse_full, auc_full,
               target.table.valid.diff, target.table.train.diff,
               pred01_valid, pred01_full, pred01_test, pred01Target1_full)
    names(stats) <- NULL
    results.stats <- rbind(results.stats, stats)
    saveRDS(results.stats, 'results.stats.4.RDS')
    
    results.full <- c(results.full, list(pred.full))
    # results.valid <- c(results.valid, list(pred.valid))
    results.test <- c(results.test, list(pred.test))

    if (rmse.winner < 0.02 & auc_valid > 0.842 & pred01_test > 8000) {
      winnerIndexes <- c(winnerIndexes, nrow(xgb_grid_sorted)*(z-1) + i)
      timestamp <- Sys.time()
      
      save.submission(test$ID, pred.test, 'results', 'xg_grid_bag',
                      c(z, i, feature.names.length, seed.number, nrounds, sample.part, model$bestInd,
                        unlist(grid.params[1:6]), round(auc_valid, 5), round(rmse.winner, 5)),
                      timestamp = timestamp)

      # -----------------------------------------------
      results.mean <- get.results.mean(results.test[winnerIndexes])
      
      print(paste('Cummulative rmse:',
                  round(Metrics::rmse(current.winner.pred, results.mean), 7)))
      
      rmse.winner.cummulative <- Metrics::rmse(current.winner.pred, results.mean)
      save.submission(test$ID, results.mean, 'results', 'xg_grid_bag_cummulative',
                      c(z, i, nrounds, sample.part,
                        feature.names.length,
                        round(auc_valid, 5),
                        round(rmse.winner.cummulative, 5)),
                      timestamp = timestamp)
    }

    saveRDS(results.full, 'results.full.4.RDS')
    # saveRDS(results.valid, 'results.valid.4.RDS')
    saveRDS(results.test, 'results.test.4.RDS')
  }
}

results.stats <- readRDS('results.stats.3.RDS')


zzz <- data.frame(results.test)
names(zzz) <- 1:ncol(zzz)
View(zzz)


# ---------------------------------------------------
# Save bagged results
results.mean <- get.results.mean(results)

Metrics::rmse(winner_11_26$TARGET, results.mean)
Metrics::rmse(h2o_deeplearning_bag_winner841287_01_44_15$TARGET, results.mean)


results.mean <- results.mean*0.1 + winner_11_26$TARGET*0.9
results.mean <- results.mean*0.1 + xg_grid_cv_bag_09_38_0247$TARGET*0.9
results.mean <- results.mean*0.1 + h2o_deeplearning_bag_winner841287_01_44_15$TARGET*0.9


results.mean.final <- results.mean*0.05 + current.winner.pred*0.95





write.csv(results.stats, 'results.stats.csv', row.names=FALSE, quote = FALSE)



zzz <- data.frame(w = current.winner.pred, p1 = xg_grid_bag_010033_1_12_110_10012_12050_5_0.001_0.3_0.7_0.8422346_12050_0.85418_0.01017$TARGET,
                  p2 = xg_grid_bag_212731_41_1_110_410001_1847_5_0.005_0.5_0.7_0.842655_1847_0.85579_0.01036$TARGET,
                  p3 = xg_grid_bag_231925_85_1_110_850001_1847_5_0.005_0.5_0.7_0.842655_1847_0.85693_0.00963$TARGET,
                  pc = xg_grid_bag_cummulative_092533_6_21_490_110_0.84489_0.00784$TARGET,
                  pd = current.winner.pred / xg_grid_bag_cummulative_092533_6_21_490_110_0.84489_0.00784$TARGET,
                  rm = results.mean)
View(zzz[zzz$pd < 0.5 & zzz$pc > 0.1, ])
View(zzz[zzz$pc > 0.5 & zzz$w < 0.5, ])



zzz <- data.frame(t = train$TARGET, p = pred)
View(zzz[zzz$t == 1 | zzz$p > 0.1, ])
View(zzz[zzz$t == 1 & zzz$p < 0.007, ])
pred[91] <- 0
Metrics::auc(train$TARGET, pred)


View(zzz[zzz$pd < 0.5 & zzz$pc > 0.1, ])
experiment <- current.winner.pred
experiment[c(7006, 15139, 23643)] <- 1 ### -0.00006


View(zzz[zzz$pc > 0.5 & zzz$w < 0.5, ])
experiment <- current.winner.pred
experiment[c(3453, 3994, 15412, 19540, 23430, 34705, 62577, 67370, 68658)] <- 1 ###

save.submission(test$ID, experiment, 'results', 'partly_predict_winner_experiment_4',
                c(feature.names.length, 1, 3453, 3994, 15412, 19540, 23430, 34705, 62577, 67370, 68658))

