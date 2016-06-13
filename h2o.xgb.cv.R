library(xgboost)
library(Matrix)
library(Metrics)
library(methods)
library(caret)
options(scipen=999)


# ---------------------------------------------------
# Load
orig.train <- read.csv("./input/train.csv", stringsAsFactors = F)
orig.test <- read.csv("./input/test.csv", stringsAsFactors = F)
orig.test$TARGET <- -1

# ---------------------------------------------------
# Merge
merged <- rbind(orig.train, orig.test)
merged <- merged[order(merged$ID),] 

# ---------------------------------------------------
# Remove near zero columns
nearZero <- nearZeroVar(merged, saveMetrics = T, freqCut = 150000/2, uniqueCut = 10/150000 * 100)
# nearZeroRowNames <- rownames(nearZero[nearZero$nzv == T, ])
nearZeroRowNames <- rownames(nearZero[nearZero$zeroVar == T, ])
# saveRDS(nearZero, 'nearZero.rds')
# View(nearZero[nearZero$nzv == T,])

feature.names <- names(merged)
feature.names <- feature.names[!(feature.names %in% nearZeroRowNames)]
# Remove deltas and ind. It's a noise.
feature.names <- feature.names[-grep('delta', feature.names)]
feature.names <- feature.names[-grep('^ind_', feature.names)]

merged <- merged[, feature.names]

# ---------------------------------------------------
# Remove corellated features
merged.corr <- cor(merged)
# cutoff = 0.95
merged.col.corr <- findCorrelation(merged.corr, cutoff = .93, verbose = F, exact = T)
# sort(names(merged)[merged.col.corr])
merged <- merged[, -merged.col.corr]

# ---------------------------------------------------
# Remove partly identical features
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
# var38 ~ 117311
merged[which(abs(merged$var38 - 117311.0) < 1), 'var38'] <- -1

# ---------------------------------------------------
# Split
train <- merged[merged$TARGET != -1, ]
test <- merged[merged$TARGET == -1, ]




# ---------------------------------------------------
# ---------------------------------------------------
# ---------------------------------------------------
library(h2o)
# library(data.table)
library(Metrics)

h2o.init(nthreads=-1, min_mem_size = "4G", max_mem_size = "8G")
h2o.shutdown(F);

feature.names <- names(train)
feature.names <- feature.names[-match(c('ID', 'TARGET'), feature.names)]
feature.formula <- formula(paste('TARGET ~ ', paste(feature.names, collapse = ' + '), sep = ''))

train$TARGET <- factor(train$TARGET)
testHex <- as.h2o(test[, feature.names], destination_frame="test.hex")
fullHex <- as.h2o(train[, c(feature.names, 'TARGET')], destination_frame="full.hex")


xgb_grid = expand.grid(
  max_depth = c(4, 5, 6, 7),
  learn_rate = c(0.001, 0.005, 0.01),
  colsample_bytree = c(0.3, 0.5, 0.7),
  subsample = c(0.5, 0.7, 0.9)
)

xgb_grid = expand.grid(
  max_depth = c(4, 5, 6, 7),
  # learn_rate = c(0.005, 0.01),
  learn_rate = c(0.1, 0.05, 0.01),
  colsample_bytree = c(0.7),
  subsample = c(0.9)
)

xgb_grid = expand.grid(
  max_depth = c(5),
  learn_rate = c(0.01),
  colsample_bytree = c(0.5),
  subsample = c(0.7)
)

predictionsList <- list()
for (i in 1:nrow(xgb_grid)) {
  grid.params <- xgb_grid[i, ]
  cat("\n")
  cat("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
  print(grid.params)

  rounds.max <- 500
  if (grid.params$learn_rate == 0.05) {
    rounds.max <- 1000
  } else if (grid.params$learn_rate == 0.01) {
    rounds.max <- 2000
  } else if (grid.params$learn_rate == 0.005) {
    rounds.max <- 4000
  }

  for (j in 1:3) {
    seed.number <- 100000 + 100*i + j
    print(paste0("Started: ", Sys.time(), ", max rounds: ", rounds.max))

    gbmHex <- h2o.gbm(x = feature.names,
                      y = "TARGET",
                      model_id="gbmStarter.hex",
                      training_frame = fullHex,
                      # distribution="AUTO",
                      ntrees = rounds.max,
                      max_depth = grid.params$max_depth,
                      nfolds = 7,
                      # validation_frame = validHex,
                      # min_rows = 2,
                      # nbins = 100,
                      learn_rate = grid.params$learn_rate,
                      col_sample_rate = grid.params$colsample_bytree,
                      sample_rate = grid.params$subsample,
                      stopping_rounds = ceiling(log(1/grid.params$learn_rate)*5),
                      stopping_metric = 'AUC',
                      stopping_tolerance = 0.00001,
                      # keep_cross_validation_predictions = T,
                      # score_each_iteration = T,
                      balance_classes = T,
                      seed = seed.number)
    print(gbmHex@model$model_summary)
    print(gbmHex@model$cross_validation_metrics)

    xgb_grid[i, paste0('number_of_trees_', j)] <- gbmHex@model$model_summary$number_of_trees

    pred <- as.vector(h2o.predict(gbmHex, testHex)$p1)
    predictionsList <- c(predictionsList, pred)

    winner.rmse <- Metrics::rmse(winner_long_tail_174228_current_all$TARGET, pred)
    xgb_grid[i, paste0('winner_rmse_', j)] <- winner.rmse

    auc <- gbmHex@model$cross_validation_metrics@metrics$AUC
    xgb_grid[i, paste0('auc_', j)] <- auc
    print(paste('cross-validation AUC:', auc))
    if (auc > 0.839) {
      submission <- data.frame(ID = test$ID, TARGET = pred)
      submissionName <- paste("./results/h2o_xgb_grid_cv", i, j,
                              format(Sys.time(), "%H_%M_%S"),
                              seed.number, auc, rounds, 'balanced',
                              paste(unlist(grid.params), collapse = '_'),
                              sep = '_')
      submissionFile <- paste0(submissionName, ".csv")
      write.csv(submission, submissionFile, row.names=FALSE, quote = FALSE)
    }
  }

  saveRDS(xgb_grid, 'h2o_xgb_grid_4.rds')
  saveRDS(predictionsList, 'h2oPredictionsList4.rds')
}

xgb_grid <- readRDS('h2o_xgb_grid_3.rds')

# pred <- as.data.frame(h2o.predict(gbmHex, testHex))
# Metrics::rmse(winner_11_26$TARGET, pred$p1)



pred <- as.vector(h2o.predict(gbmHex, fullHex)$p1)

