library(xgboost)
library(Matrix)
library(caret)
library(Metrics)
library(methods)
require(MASS)
options(scipen=999)

get.sign <- function(v) {
  if (v > 0) {
    if (v == 0) {
      return(0)
    }
    return(1)
  }
  return(-1)
}
get.log <- function(v) {
  l <- log(abs(v) + 1)
  sign <- get.sign(v)
  if (sign < 0) {
    return(-l)
  }
  return(l)
}


# ---------------------------------------------------
# Load
orig.train <- read.csv("./input/train.csv", stringsAsFactors = F)
orig.test <- read.csv("./input/test.csv", stringsAsFactors = F)

# ---------------------------------------------------
# Merge
orig.test$TARGET <- -1
merged <- rbind(orig.train, orig.test)


# ---------------------------------------------------
# ---------------------------------------------------
# Remove near zero columns
nearZero <- nearZeroVar(merged, saveMetrics = T, freqCut = 150000/2, uniqueCut = 10/150000 * 100)
# nearZeroRowNames <- rownames(nearZero[nearZero$nzv == T, ])
nearZeroRowNames <- rownames(nearZero[nearZero$zeroVar == T, ])
# saveRDS(nearZero, 'nearZero.rds')
# View(nearZero[nearZero$nzv == T,])

feature.names <- names(merged)
feature.names <- feature.names[!(feature.names %in% nearZeroRowNames)]
# Remove deltas. It's a noise.
feature.names <- feature.names[-grep('delta', feature.names)]
merged <- merged[, feature.names]

# # ---------------------------------------------------
# # Get all cash columns
# feature.train.names <- names(merged)
# cash.column.names <- c()
# for (f in feature.train.names) {
#   if (class(merged[[f]]) == "numeric") {
#     if (length(unique(merged[[f]])) > 100) {
#       cash.column.names <- c(cash.column.names, f)
#     }
#   }
# }
# 
# # ---------------------------------------------------
# # Calc deltas
# feature.names <- names(merged)
# saldo.vars <- feature.names[grep('saldo_', feature.names)]
# # saldo.vars
# # View(train[, c('ID', 'TARGET', saldo.vars)])
# deltas.feature.names <- saldo.vars
# # deltas.feature.names <- unique(c(saldo.vars, cash.column.names))
# 
# for(fn1.index in 1:(length(deltas.feature.names)-1)) {
#   for(fn2.index in (fn1.index+1):length(deltas.feature.names)) {
#     fn1.name <- deltas.feature.names[fn1.index]
#     fn2.name <- deltas.feature.names[fn2.index]
#     delta <- merged[[fn1.name]] - merged[[fn2.name]]
#     if (sum(delta) > 10) {
#       merged[[paste('d', fn1.name, fn2.name, sep = '__')]] <- delta
#     }
#   }
# }


# ---------------------------------------------------
# Remove corellated features
merged.corr <- cor(merged)
merged.col.corr <- findCorrelation(merged.corr, cutoff = .95, verbose = F, exact = T)
# sort(names(merged)[merged.col.corr])
merged <- merged[, -merged.col.corr]

# ---------------------------------------------------
# Remove identical features
merged.chunk <- merged[1:500, ]
feature.names <- names(merged)
feature.names <- feature.names[-match(c('ID', 'TARGET'), feature.names)]

for(fn1 in feature.names[1:(length(feature.names)-1)]) {
  feature.names.2 <- names(merged)
  feature.names.2 <- feature.names.2[-match(c('ID', 'TARGET'), feature.names.2)]
  fn1.index <- match(fn1, feature.names.2)
  if (!is.na(fn1.index)) {
    # print(paste(fn1.index, fn1, sep = ' : '))
    fn1.values <- merged.chunk[[fn1]]
    for(fn2 in feature.names.2[(fn1.index+1):length(feature.names.2)]) {
      fn2.values <- merged.chunk[[fn2]]
      if (all(fn1.values == fn2.values)) {
        # cat(fn1, "and", fn2, "are equals.\n")
        merged[[fn2]] <- NULL
      }
    }
  }
}
rm(merged.chunk)
gc()


# ---------------------------------------------------
# Convert
# feature.train.names <- names(merged)[-1]
# feature.train.names <- feature.train.names[-length(feature.train.names)]
# for (f in feature.train.names) {
#   #   if (class(merged[[f]]) == "numeric") {
#   #     merged[[f]] <- merged[[f]] / max(merged[[f]])
#   #   }
#   if (length(unique(merged[[f]])) < 10) {
#     merged[[paste0(f, '_factor')]] <- factor(merged[[f]])
#   }
# }
# 
# merged$var15_factor <- factor(merged$var15)
# merged$var15 <- NULL
# merged$var38 <- log(merged$var38 + 1)
# merged$saldo_var30 <- sapply(merged$saldo_var30, get.log)
# merged$saldo_medio_var5_hace3 <- sapply(merged$saldo_medio_var5_hace3, get.log)
# merged$saldo_medio_var5_hace2 <- sapply(merged$saldo_medio_var5_hace2, get.log)
# merged$saldo_medio_var5_ult3 <- sapply(merged$saldo_medio_var5_ult3, get.log)
# merged$saldo_medio_var5_ult1 <- sapply(merged$saldo_medio_var5_ult1, get.log)
# merged$num_var45_hace3 <- sapply(merged$num_var45_hace3, get.log)
# merged$num_var45_ult3 <- sapply(merged$num_var45_ult3, get.log)
# merged$num_var45_hace2 <- sapply(merged$num_var45_hace2, get.log)



# # ---------------------------------------------------
# # The mode of var38 (117311.0)
# zzz <- table(merged$var38)
# zzz <- sort(zzz, decreasing = T)
# head(zzz)
merged[which(abs(merged$var38 - 117311.0) < 1), 'var38'] <- -1
# merged[merged$var38 == 0, 'var38'] <- -100
# merged[merged$var38 == 0, 'var38'] <- 117311.0
# merged[merged$var38 == -100, 'var38'] <- 0
# merged$var38_log <- log(merged$var38 + 1)

# ---------------------------------------------------
# var15values <- unique(merged$var15)
# for(val in var15values) {
#   merged[[paste0('var15_', val)]] <- ifelse(merged$var15 == val, 1, 0)
# }
# merged$var15 <- NULL

# ---------------------------------------------------
# Split
train <- merged[merged$TARGET != -1, ]
test <- merged[merged$TARGET == -1, ]


# ---------------------------------------------------
# ---------------------------------------------------
# Features
feature.names <- names(train)
feature.names <- feature.names[-match(c('ID', 'TARGET'), feature.names)]
# feature.names <- feature.names[-grep('d__.*?var30.*', feature.names)]
# feature.names <- feature.names[-grep('d__.*', feature.names)]
feature.formula <- formula(paste('TARGET ~ ', paste(feature.names, collapse = ' + '), sep = ''))


# dfull <-  xgb.DMatrix(data.matrix(feature.formula, data = train), label = train$TARGET)
dtest <- data.matrix(test[, feature.names])
sparsed.feature.names <- colnames(dtest)

# ---------------------------------------------------
# XGBOOST
i = 1
results <- list()
mixed.winner.mean = h2o_deeplearning_bag_winner841287_01_44_15$TARGET
winner.target <- h2o_deeplearning_bag_winner841287_01_44_15$TARGET
for (i in 1:1000) {
  print(i)
  print(paste0("Started: ", Sys.time()))
  
  set.seed(1000 + i)
  indexes <- sample(seq_len(nrow(train)), floor(nrow(train)*0.8))
  dtrain <- xgb.DMatrix(data.matrix(train[indexes, feature.names]),
                        label = train[indexes, 'TARGET'])
  dvalid <- xgb.DMatrix(data.matrix(train[-indexes, feature.names]),
                        label = train[-indexes, 'TARGET'])
  watchlist <- list(valid = dvalid, train = dtrain)

#   rand.max.depth <- round(runif(1, 3, 8), 0)
#   rand.eta <- round(runif(1, 0.008, 0.12), 3)
#   rand.colsample_bytree <- round(runif(1, 0.4, 0.9), 2)
#   print(paste0("Max depth: ", rand.max.depth, ', eta: ', rand.eta, ', colsample_bytree: ', rand.colsample_bytree))
#   
#   params <- list(booster = "gbtree", objective = "binary:logistic",
#                  max_depth = rand.max.depth, eta = rand.eta,
#                  colsample_bytree = rand.colsample_bytree, subsample = 0.95)
  #   params <- list(booster = "gbtree", objective = "binary:logistic",
  #                  max_depth = 6, eta = 0.1,
  #                  colsample_bytree = 0.5, subsample = 0.95)
  params <- list(booster = "gbtree", objective = "binary:logistic",
                 max_depth = 5, eta = 0.005,
                 # scale_pos_weight = 0.1,
                 # min_child_weight = 10,
                 # max_delta_step = 2,
                 # gamma = 10,
                 colsample_bytree = 0.5, subsample = 0.7)
  set.seed(1000 + i)
  model <- xgb.train(params = params,
                     data = dtrain,
                     nrounds = 3001,
                     early.stop.round = 200,
                     eval_metric = 'auc',
                     maximize = T,
                     watchlist = watchlist,
                     print.every.n = 500,
                     verbose = T)
  print(paste0("Model created: ", Sys.time(), ', best AUC: ', model$bestScore))
#   imp <- xgb.importance(sparsed.feature.names, model = model)
#   print(head(imp, 20))

  if (model$bestScore > 0.840) {
    pred <- predict(model, dtest)
    print(paste("Winner RMSE:", round(Metrics::rmse(winner.target, pred), 7),
                ", mean: ", round(Metrics::rmse(winner.target, (winner.target + pred)/2), 7),
                ", 8 to 2: ", round(Metrics::rmse(winner.target, winner.target*0.8 + pred*0.2), 7),
                ", 2 to 8: ", round(Metrics::rmse(winner.target, winner.target*0.2 + pred*0.8), 7)
    ))

    if (model$bestScore > 0.842) {
      submission <- data.frame(ID = test$ID, TARGET = pred)
      submissionName <- paste0("./results/xg_simple_bag_", i, '_', format(Sys.time(), "%H_%M_%S"), '_5_0005_05_07')
      submissionFile <- paste0(submissionName, ".csv")
      write.csv(submission, submissionFile, row.names = FALSE, quote = FALSE)

      results <- c(results, list(pred))
      results.len <- length(results)
      results.mean <- unlist(c(results[1]))
      for(index in 2:results.len) {
        results.mean <- cbind(results.mean, unlist(c(results[index])))
      }
      results.mean <- rowMeans(results.mean)
      mixed.winner.mean <- mixed.winner.mean*0.9 + pred*0.1
      print(paste('Cummulative winner RMSE:', round(Metrics::rmse(winner.target, results.mean), 7),
                  ", mean: ", round(Metrics::rmse(winner.target, (winner.target + results.mean)/2), 7),
                  ", mixed: ", round(Metrics::rmse(winner.target, mixed.winner.mean), 7)
      ))
    }
  }
}


results.mean <- unlist(c(results[1]))
for(index in 2:length(results)) {
  results.mean <- cbind(results.mean, unlist(c(results[index])))
}
results.mean <- rowMeans(results.mean)


pred <- predict(model, dvalid)
Metrics::auc(train[-indexes, 'TARGET'], pred)
View(data.frame(target = train[-indexes, 'TARGET'], pred = pred))


submission <- xg_bag_19_43_30_ensemble
submission$TARGET <- submission$TARGET*0.9 + results.mean*0.1
Metrics::rmse(xg_bag_19_43_30_ensemble$TARGET, submission$TARGET)


submission <- h2o_deeplearning_bag_winner841287_01_44_15
# submission$TARGET <- h2o_deeplearning_bag_winner841287_01_44_15$TARGET*0.9 + pred*0.1
# Metrics::rmse(h2o_deeplearning_bag_winner841287_01_44_15$TARGET, submission$TARGET)
submission$h2o_deeplearning_20_07_49 <- h2o_deeplearning_20_07_49$TARGET
submission$pred <- pred
submission$xg_grid_cv_bag_09_38_0247 <- xg_grid_cv_bag_09_38_0247$TARGET
submission$winner_11_26 <- winner_11_26$TARGET
submission$xg_grid_cv_bag_10_03_0594 <- xg_grid_cv_bag_10_03_0594$TARGET

Metrics::rmse(submission$pred, submission$pred_3)


# ---------------------------------------------------
# SAVE
submission <- data.frame(ID = test$ID, TARGET = results.mean)
submission <- data.frame(ID = test$ID, TARGET = pred)
submissionName <- paste0("./results/xg_bag_", format(Sys.time(), "%H_%M_%S"), '_wo_d_var30_n50_d8')
submissionName <- paste0("./results/xg_bag_", format(Sys.time(), "%H_%M_%S"), '_rand_', length(results))
submissionName <- paste0("./results/xg_bag_", format(Sys.time(), "%H_%M_%S"), '_ensemble')
submissionFile <- paste0(submissionName, ".csv")
write.csv(submission, submissionFile, row.names=FALSE, quote = FALSE)

