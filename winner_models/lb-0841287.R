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
# ---------------------------------------------------
# Remove near zero columns
nearZero <- nearZeroVar(merged, saveMetrics = T, freqCut = 150000/2, uniqueCut = 10/150000 * 100)
# nearZeroRowNames <- rownames(nearZero[nearZero$nzv == T, ])
nearZeroRowNames <- rownames(nearZero[nearZero$zeroVar == T, ])
# saveRDS(nearZero, 'nearZero.rds')
# View(nearZero[nearZero$nzv == T,])

feature.names <- names(merged)
feature.names <- feature.names[!(feature.names %in% nearZeroRowNames)]
merged <- merged[, feature.names]


# ---------------------------------------------------
# Remove corellated features
merged.corr <- cor(merged)
merged.col.corr <- findCorrelation(merged.corr, cutoff = .95, verbose = F, exact = T)
# names(merged)[merged.col.corr]
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
merged[which(abs(merged$var38 - 117311.0) < 1), 'var38'] <- -1

# ---------------------------------------------------
# Split
train <- merged[merged$TARGET != -1, ]
test <- merged[merged$TARGET == -1, ]



# ---------------------------------------------------
# Features
feature.names <- names(train)
feature.names <- feature.names[-match(c('ID', 'TARGET'), feature.names)]
feature.names <- feature.names[-grep('delta', feature.names)]
feature.formula <- formula(paste('TARGET ~ ', paste(feature.names, collapse = ' + '), sep = ''))

# ---------------------------------------------------
# XGBOOST
dfull <- xgb.DMatrix(data.matrix(train[, feature.names]), label = train$TARGET)
dtest <- data.matrix(test[, feature.names])


xgb_grid_final = expand.grid(
  max_depth = c(5, 6),
  eta = c(0.005, 0.01),
  colsample_bytree = c(0.5, 0.7),
  subsample = c(0.7, 0.8)
)

xgb_grid_final$steps <- sapply(xgb_grid_final$eta, function(eta) {
  if (eta == 0.005) return(1900)
  else if (eta == 0.01) return(900)
  return(300)
})

results <- c()
for (i in 1:nrow(xgb_grid_final)) {
  print(paste0("Started: ", Sys.time()))
  
  grid.params <- xgb_grid_final[i, ]
  print(grid.params)
  
  xgb_params <- list(booster = "gbtree", objective = "binary:logistic",
                     eval_metric = "auc",
                     max_depth = grid.params$max_depth,
                     eta = grid.params$eta,
                     colsample_bytree = grid.params$colsample_bytree,
                     subsample = grid.params$subsample)
  for (j in 1:5) {
    seed.number <- 1000*i + j
    set.seed(seed.number)
    
    model <- xgb.train(
      data = dfull,
      params = c(xgb_params),
      nrounds = grid.params$steps)
    pred <- predict(model, dtest)
    
    rmse_winner_3 <- Metrics::rmse(winner_11_26$TARGET, pred)
    print(paste("Winner RMSE:", round(rmse_winner_3, 5)));
    
    if (rmse_winner_3 < 0.007) {
      results <- c(results, list(pred))
      
      submission <- data.frame(ID = test$ID, TARGET = pred)
      submissionName <- paste("./results/xg_gradient", i, j,
                              format(Sys.time(), "%H_%M_%S"),
                              length(feature.names), seed.number,
                              paste(unlist(xgb_params[-c(1,2,3)]), collapse = '_'),
                              round(rmse_winner_3, 5),
                              sep = '_')
      submissionFile <- paste0(submissionName, ".csv")
      write.csv(submission, submissionFile, row.names=FALSE, quote = FALSE)
      
      results.mean <- unlist(c(results[1]))
      for(index in 2:length(results)) {
        results.mean <- cbind(results.mean, unlist(c(results[index])))
      }
      results.mean <- rowMeans(results.mean)
      print(paste('Cummulative rmse:', Metrics::rmse(winner_11_26$TARGET, results.mean)))
    }
  }
}


# ---------------------------------------------------
# Save bagged results
results.mean <- unlist(c(results[1]))
for(index in 2:length(results)) {
  # for(index in 2:11) {
  results.mean <- cbind(results.mean, unlist(c(results[index])))
}
results.mean <- rowMeans(results.mean)

Metrics::rmse(winner_11_26$TARGET, results.mean)


results.mean <- results.mean*0.3 + winner_11_26$TARGET*0.7

submission <- data.frame(ID = test$ID, TARGET = results.mean)
submissionName <- paste0("./results/xg_grid_cv_bag_", format(Sys.time(), "%H_%M_%S"), length(results))
submissionFile <- paste0(submissionName, ".csv")
write.csv(submission, submissionFile, row.names=FALSE, quote = FALSE)


