# ===============================================================
#    max_depth  eta colsample_bytree subsample rmse_winner seed      auc auc_index
# 26         5 0.01              0.5       0.7 0.005015697 1026 0.842781       895
#
# LB - 0.841003
#
library(xgboost)
library(Matrix)
library(caret)
library(Metrics)
library(methods)
require(MASS)
options(scipen=999)

# ---------------------------------------------------
# Load
orig.train <- read.csv("./input/train.csv", stringsAsFactors = F)
orig.test <- read.csv("./input/test.csv", stringsAsFactors = F)

# ---------------------------------------------------
# Merge
orig.test$TARGET <- -1
merged <- rbind(orig.train, orig.test)


# ---------------------------------------------------
# Remove near zero columns
nearZero <- nearZeroVar(merged, saveMetrics = T, freqCut = 150000/2, uniqueCut = 10/150000 * 100)
nearZeroRowNames <- rownames(nearZero[nearZero$zeroVar == T, ])

feature.names <- names(merged)
feature.names <- feature.names[!(feature.names %in% nearZeroRowNames)]
merged <- merged[, feature.names]

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
# Split
train <- merged[merged$TARGET != -1, ]
test <- merged[merged$TARGET == -1, ]


# ---------------------------------------------------
# ---------------------------------------------------
# XGBOOST Cross Validation

xgb_grid = expand.grid(
  max_depth = c(4, 5, 6),
  eta = c(0.05, 0.03, 0.01),
  colsample_bytree = c(0.5, 0.7),
  subsample = c(0.6, 0.7, 0.8, 0.9)
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
  seed.number <- 1000 + i
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
    
#     xgb_grid[i, 'rmse_winner'] <- Metrics::rmse(xg_11_winner$TARGET, pred)
#     print(paste("Winner RMSE:", xgb_grid[i, 'rmse_winner']));
#     
#     xgb_grid[i, 'rmse_winner_2'] <- Metrics::rmse(xg_26_winner$TARGET, pred)
#     print(paste("Winner 2 RMSE:", xgb_grid[i, 'rmse_winner_2']));
#     
#     xgb_grid[i, 'rmse_winner_3'] <- Metrics::rmse(winner_11_26$TARGET, pred)
#     print(paste("Winner 3 RMSE:", xgb_grid[i, 'rmse_winner_3']));
    
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
