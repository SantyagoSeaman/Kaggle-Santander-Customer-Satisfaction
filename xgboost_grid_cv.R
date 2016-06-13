library(xgboost)
library(Matrix)
library(caret)
library(Metrics)
library(methods)
library(caret)
options(scipen=999)

# ---------------------------------------------------
# Grid params
# xgb_grid = expand.grid(
#   max_depth = c(3, 4, 5, 6),
#   eta = c(0.07, 0.05, 0.03, 0.01),
#   colsample_bytree = c(0.5, 0.7),
#   subsample = c(0.8, 0.9, 1, 0.6, 0.7)
# )

xgb_grid = expand.grid(
  max_depth = c(4, 5, 6),
  eta = c(0.005, 0.01, 0.03),
  colsample_bytree = c(0.5, 0.7, 0.9),
  subsample = c(0.7, 0.8, 0.9)
)

# ---------------------------------------------------
# Features
feature.names <- names(train)
feature.names <- feature.names[-match(c('ID', 'TARGET'), feature.names)]
# feature.names <- feature.names[grep('^var', feature.names)]
# feature.names <- feature.names[grep('saldo', feature.names)]
feature.names <- feature.names[-grep('delta', feature.names)]
# feature.names <- feature.names[-grep('^imp_', feature.names)]
# feature.names <- feature.names[-grep('^ind_', feature.names)]
# feature.names <- feature.names[-grep('^num_', feature.names)]
# feature.names <- feature.names[-grep('d__.*?var30.*', feature.names)]
# feature.names <- feature.names[-grep('d__.*', feature.names)]
feature.formula <- formula(paste('TARGET ~ ', paste(feature.names, collapse = ' + '), sep = ''))

# ---------------------------------------------------
# XGBOOST
dfull <- xgb.DMatrix(data.matrix(train[, feature.names]), label = train$TARGET)
dtest <- data.matrix(test[, feature.names])

results <- list()
best.params <- list()
best.seednumber <- 0
best.auc <- 0
best.auc.index <- 0
for (i in 1:nrow(xgb_grid)) {

  grid.params <- xgb_grid[i, ]
  print(grid.params)

  xgb_params <- list(booster = "gbtree", objective = "binary:logistic",
                     eval_metric = "auc",
                     # scale_pos_weight = 0.1,
                     max_depth = grid.params$max_depth,
                     eta = grid.params$eta,
                     colsample_bytree = grid.params$colsample_bytree,
                     subsample = grid.params$subsample)
  seed.number <- 100000 + i
  set.seed(seed.number)
  xgb_cv = xgb.cv(params = xgb_params,
                  data = dfull,
                  nrounds = 2001,
                  nfold = 7,            # number of folds in K-fold
                  prediction = F,       # return the prediction using the final model 
                  showsd = F,           # standard deviation of loss across folds
                  stratified = F,       # sample is unbalanced; use stratified sampling
                  verbose = T,
                  print.every.n = 100,
                  early.stop.round = 20
  )

  max.auc <- max(xgb_cv$test.auc.mean)
  max.auc.index <- which.max(xgb_cv$test.auc.mean)

  xgb_grid[i, 'seed'] <- seed.number
  xgb_grid[i, 'auc'] <- max.auc
  xgb_grid[i, 'auc_index'] <- max.auc.index

  if (max.auc > best.auc) {
    best.auc = max.auc
    best.auc.index = max.auc.index
    best.seednumber = seed.number
    best.params = c(xgb_params)
  }

  if (max.auc > 0.840) {
    set.seed(seed.number)
    model <- xgb.train(data = dfull, params = c(xgb_params), nrounds = max.auc.index)
    pred <- predict(model, dtest)
    results <- c(results, list(pred))

    xgb_grid[i, 'rmse_winner'] <- Metrics::rmse(xg_11_winner$TARGET, pred)
    xgb_grid[i, 'rmse_winner_2'] <- Metrics::rmse(xg_26_winner$TARGET, pred)
    xgb_grid[i, 'rmse_winner_3'] <- Metrics::rmse(winner_11_26$TARGET, pred)

    print(paste("Winner RMSE:", round(xgb_grid[i, 'rmse_winner'], 5),
                round(xgb_grid[i, 'rmse_winner_2'], 5),
                round(xgb_grid[i, 'rmse_winner_3'], 5)));

    if (max.auc > 0.842) {
      submission <- data.frame(ID = test$ID, TARGET = pred)
      submissionName <- paste("./results/xg_grid_cv_max", i,
                              format(Sys.time(), "%H_%M_%S"),
                              length(feature.names), seed.number, max.auc, max.auc.index,
                              paste(unlist(xgb_params[-c(1,2,3)]), collapse = '_'),
                              sep = '_')
      submissionFile <- paste0(submissionName, ".csv")
      write.csv(submission, submissionFile, row.names=FALSE, quote = FALSE)
    }  
#     mean.valid <- c()
#     for (j in 1:5) {
#       set.seed(100 + j)
#       indexes <- sample(seq_len(nrow(train)), floor(nrow(train)*0.7))
#       dvalid <- xgb.DMatrix(data.matrix(train[-indexes, feature.names]),
#                             label = train[-indexes, 'TARGET'])
#       set.seed(seed.number)
#       pred <- predict(model, dvalid)
#       auc.valid <- Metrics::auc(train[-indexes, 'TARGET'], pred)
#       # print(auc.valid)
#       mean.valid <- c(mean.valid, auc.valid)
#     }
#     xgb_grid[i, 'auc_valid_mean'] <- round(mean(mean.valid), 5)
#     print(paste('Mean valid AUC:', xgb_grid[i, 'auc_valid_mean']));
  } else {
    print("")
  }

  print(paste("Best test AUC:", max.auc));
  print("===========================================")
}

# ---------------------------------------------------
# Predict best model
set.seed(best.seednumber)
model <- xgb.train(data = dfull, params = c(best.params), nrounds = best.auc.index)
imp <- xgb.importance(feature.names, model = model)
print(head(imp, 20))

# ---------------------------------------------------
# Check best model
for (i in 1:10) {
  set.seed(100 + i)
  indexes <- sample(seq_len(nrow(train)), floor(nrow(train)*0.7))
  dvalid <- xgb.DMatrix(data.matrix(train[-indexes, feature.names]),
                        label = train[-indexes, 'TARGET'])
  pred <- predict(model, dvalid)
  print(Metrics::auc(train[-indexes, 'TARGET'], pred))
}

# ---------------------------------------------------
# Predict best
pred <- predict(model, dtest)

# ---------------------------------------------------
# Save best
submission <- data.frame(ID = test$ID, TARGET = pred)
submissionName <- paste("./results/xg_grid_cv_best",
                        format(Sys.time(), "%H_%M_%S"),
                        length(feature.names), best.seednumber,
                        best.auc, best.auc.index,
                        paste(unlist(best.params[-c(1,2)]), collapse = '_'),
                        sep = '_')
submissionFile <- paste0(submissionName, ".csv")
write.csv(submission, submissionFile, row.names=FALSE, quote = FALSE)

# ---------------------------------------------------
# Save bagged results
results.mean <- unlist(c(results[1]))
for(index in 2:length(results)) {
  results.mean <- cbind(results.mean, unlist(c(results[index])))
}
results.mean <- rowMeans(results.mean)

submission <- data.frame(ID = test$ID, TARGET = results.mean)
submissionName <- paste0("./results/xg_grid_cv_bag_", format(Sys.time(), "%H_%M_%S"), length(results))
submissionFile <- paste0(submissionName, ".csv")
write.csv(submission, submissionFile, row.names=FALSE, quote = FALSE)
