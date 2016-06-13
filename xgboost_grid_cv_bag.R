library(xgboost)
library(Matrix)
library(Metrics)
library(methods)
library(caret)
options(scipen=999)
source('./helpers.r')

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
feature.names <- c(feature.names, 'ind_var33_0', 'ind_var20_0')

merged <- merged[, feature.names]

# ---------------------------------------------------
# Remove corellated features
merged.corr <- cor(merged)
# cutoff = 0.95
merged.col.corr <- findCorrelation(merged.corr, cutoff = .93, verbose = F, exact = T)
# merged.col.corr <- findCorrelation(merged.corr, cutoff = .99, verbose = F, exact = T)
# sort(names(merged)[merged.col.corr])
merged <- merged[, -merged.col.corr]

# ---------------------------------------------------
feature.names <- names(merged)
feature.names <- feature.names[-match(c('ID', 'TARGET'), feature.names)]
count0 <- function(x) {
  return( sum(x == 0) )
}
merged$count0 <- apply(merged[, feature.names], 1, FUN=count0)


# ---------------------------------------------------
# Remove partly identical features
merged.chunk <- merged[1:1000, ] #1:500
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


# # ---------------------------------------------------
# # var38 ~ 117311
# merged[which(abs(merged$var38 - 117311.0) < 1), 'var38'] <- -1
# # merged$na_var38 <- 0
# # merged[merged$var38 == -1, 'na_var38'] <- 1

# ---------------------------------------------------
merged$var38 <- log(merged$var38)
merged[which(abs(merged$var38 - 11.672583) < 0.000001), 'var38'] <- -1
# nrow(merged[merged$var38 == -1,])


# ---------------------------------------------------
# Split
train <- merged[merged$TARGET != -1, ]
test <- merged[merged$TARGET == -1, ]




# ---------------------------------------------------
# Grid params
# Try to find combinations of xgboost parameters with the most exact result

# xgb_grid = expand.grid(
#   max_depth = c(3, 4, 5, 6),
#   eta = c(0.07, 0.05, 0.03, 0.01),
#   colsample_bytree = c(0.5, 0.7),
#   subsample = c(0.8, 0.9, 1, 0.6, 0.7)
# )

xgb_grid = expand.grid(
  max_depth = c(4, 5, 6),
  eta = c(0.001, 0.005, 0.01, 0.03),
  colsample_bytree = c(0.3, 0.5, 0.7),
  subsample = c(0.7, 0.8, 0.9)
)

xgb_grid = expand.grid(
  max_depth = c(5),
  eta = c(0.005),
  colsample_bytree = c(0.5),
  subsample = c(0.7)
)

# ---------------------------------------------------
# Features
feature.names <- names(train)
feature.names <- feature.names[-match(c('ID', 'TARGET'), feature.names)]
feature.formula <- formula(paste('TARGET ~ ', paste(feature.names, collapse = ' + '), sep = ''))

# ---------------------------------------------------
# XGBOOST
dfull <- xgb.DMatrix(data.matrix(train[, feature.names]), label = train$TARGET)
dtest <- data.matrix(test[, feature.names])

# -----------------------------------------------------------------------------
rounds <- 10
xgb_grid_results <- data.frame(i = 0, j = 0, max_depth = 0, eta = 0,
                               colsample_bytree = 0, subsample = 0,
                               seed = 0, auc = 0, step = 0)
# xgb_grid_results <- xgb_grid_results[0,]

for (i in 1:nrow(xgb_grid)) {
  print(paste0("Started: ", Sys.time()))

  grid.params <- xgb_grid[i, ]
  xgb_params <- list(booster = "gbtree", objective = "binary:logistic",
                     eval_metric = "auc",
                     # scale_pos_weight = 0.1,
                     # min_child_weight = 10,
                     max_depth = grid.params$max_depth,
                     eta = grid.params$eta,
                     colsample_bytree = grid.params$colsample_bytree,
                     subsample = grid.params$subsample)

  for (j in 1:rounds) {
    seed.number <- 100000 + 5000*i + j
    set.seed(seed.number)

    xgb_cv = xgb.cv(params = xgb_params,
                    data = dfull,
                    nrounds = 20000,
                    nfold = 5,
                    prediction = F,
                    showsd = F,
                    stratified = F,
                    verbose = T,
                    print.every.n = 100,
                    early.stop.round = ceiling(log(1/grid.params$eta)*30))

    max.auc <- max(xgb_cv$test.auc.mean)
    max.auc.step <- which.max(xgb_cv$test.auc.mean)

    xgb_grid_results <- rbind(xgb_grid_results,
                              unlist(c(i, j, grid.params[1:4], seed.number, max.auc, max.auc.step)))
  }
  # Harcoded
  if (i == 1) xgb_grid_results <- xgb_grid_results[-1, ]

  xgb_grid[i, 'mean_auc'] <- mean(xgb_grid_results[xgb_grid_results$i == i & xgb_grid_results$auc > 0.841, 'auc'])
  xgb_grid[i, 'mean_step'] <- mean(xgb_grid_results[xgb_grid_results$i == i & xgb_grid_results$auc > 0.841, 'step'])

  print(paste0("Calculated: ", Sys.time()))
  print(xgb_grid_results[xgb_grid_results$i == i, ])
  cat("-----------------------------------------------------------\n")
  print(xgb_grid[i, ])
  cat("=======================================================================\n")
  
  saveRDS(xgb_grid, 'xgb_grid_5_top.rds')
  saveRDS(xgb_grid_results, 'xgb_grid_results_5_top.rds')
}



# xgb_grid <- readRDS('xgb_grid_3.rds')
# xgb_grid_results <- readRDS('xgb_grid_results_3.rds')


# ---------------------------------------------------
# Bagging with the most exact parameters

# xgb_grid_final = expand.grid(
#   max_depth = c(5),
#   eta = c(0.005),
#   colsample_bytree = c(0.5),
#   subsample = c(0.7)
# )
# 
# xgb_grid_final$steps <- sapply(xgb_grid_final$eta, function(eta) {
#   if (eta == 0.005) return(1850)
#   else if (eta == 0.01) return(900)
#   else if (eta == 0.001) return(8000)
#   return(400)
# })

xgb_grid_sorted <- xgb_grid[order(xgb_grid$mean_auc, decreasing = T, na.last = T), ]
xgb_grid_sorted <- xgb_grid_sorted[!is.na(xgb_grid_sorted$mean_auc) & xgb_grid_sorted$mean_auc > 0.8422, ]
xgb_grid_sorted$mean_step <- ceiling(xgb_grid_sorted$mean_step)

current.winner.pred <- winner_long_tail_101929_saldo_medio_var12_hace2_700000$TARGET
feature.names.length <- length(feature.names)
results <- c()
rmse.winner <- 1000
for(z in 1:100) {
  for (i in 1:nrow(xgb_grid_sorted)) {
    print(paste0("+++ Round: ", z, "-", i, "  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"))

    grid.params <- xgb_grid_sorted[i, ]
    print(grid.params)

    xgb_params <- list(booster = "gbtree", objective = "binary:logistic",
                       eval_metric = "auc",
                       max_depth = grid.params$max_depth,
                       eta = grid.params$eta,
                       # min_child_weight = 10,
                       colsample_bytree = grid.params$colsample_bytree,
                       subsample = grid.params$subsample)

    mean_step <- grid.params$mean_step
    for (nrounds in seq(mean_step - ceiling(mean_step/2), mean_step + ceiling(mean_step/2), length.out = 10)) {
      nrounds <- ceiling(nrounds)
      seed.number <- 10000000*z + 100000*i + nrounds
      print(paste0("--- Started: ", Sys.time(), ", rounds: ", nrounds, ', seed: ', seed.number, ' --------'))

      set.seed(seed.number)
      model <- xgb.train(
        data = dfull,
        params = c(xgb_params),
        nrounds = nrounds)
      imp <- xgb.importance(feature.names, model= model)

      pred <- predict(model, dfull)
      rmse_train <- Metrics::rmse(as.integer(as.character(train$TARGET)), pred)
      auc_train <- Metrics::auc(train$TARGET, pred)

      # Compare with prev. winners    
      rmse.winner.prev <- rmse.winner
      pred <- predict(model, dtest)
      rmse.winner <- Metrics::rmse(current.winner.pred, pred)

      print(paste("Winner RMSE:",
                  round(rmse.winner, 7),
                  ", Train RMSE:", round(rmse_train, 7),
                  ", Train AUC:", round(auc_train, 7)
      ));

      if (rmse.winner < 0.01) {
        results <- c(results, list(pred))
  
        save.submission(test$ID, pred, 'results', 'xg_grid',
                        c(z, i, feature.names.length, seed.number, nrounds,
                          unlist(grid.params), round(rmse.winner, 5)))

        # -----------------------------------------------
        results.len <- length(results)
        results.mean <- get.results.mean(results)

        print(paste('Cummulative rmse:',
                    round(Metrics::rmse(current.winner.pred, results.mean), 7)))
  
        rmse.winner.cummulative <- Metrics::rmse(current.winner.pred, results.mean)
        save.submission(test$ID, results.mean, 'results', 'xg_grid_cummulative',
                        c(z, i, nrounds, feature.names.length, round(rmse.winner.cummulative, 5)))
      }
      
      if (rmse.winner / rmse.winner.prev > 1.05) {
        break;
      }
    }
  }
}

# ---------------------------------------------------
# Save bagged results
results.mean <- get.results.mean(results)

Metrics::rmse(winner_11_26$TARGET, results.mean)
Metrics::rmse(h2o_deeplearning_bag_winner841287_01_44_15$TARGET, results.mean)


results.mean <- results.mean*0.1 + winner_11_26$TARGET*0.9
results.mean <- results.mean*0.1 + xg_grid_cv_bag_09_38_0247$TARGET*0.9
results.mean <- results.mean*0.1 + h2o_deeplearning_bag_winner841287_01_44_15$TARGET*0.9


results.mean.final <- results.mean*0.05 + current.winner.pred*0.95

rmse.winner.cummulative <- Metrics::rmse(current.winner.pred, results.mean.final)
save.submission(test$ID, results.mean.final, 'results', 'xg_grid_cummulative_final_ens',
                c(feature.names.length, 0.05, 0.95, round(rmse.winner.cummulative, 5)))

