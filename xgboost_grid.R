
# ---------------------------------------------------
# Params grid
xgb_grid_1 = expand.grid(
  # nrounds = c(100, 300, 500),
  seed = c(1, 2, 3),
  # scale_pos_weight = 0.1,
  max_depth = c(5, 6, 7, 8),
  eta = c(0.1, 0.05, 0.01),
  colsample_bytree = c(0.5, 0.7, 0.9)
)

# ---------------------------------------------------
# Features
feature.names <- names(train)
feature.names <- feature.names[-match(c('ID', 'TARGET'), feature.names)]
feature.names <- feature.names[-grep('d__.*?var30.*', feature.names)]
feature.names <- feature.names[-grep('d__.*', feature.names)]
feature.formula <- formula(paste('TARGET ~ ', paste(feature.names, collapse = ' + '), sep = ''))

# dfull <-  xgb.DMatrix(data.matrix(feature.formula, data = train), label = train$TARGET)
dtest <- data.matrix(test[, feature.names])
sparsed.feature.names <- colnames(dtest)


# The mode of var38 (117311.0)
# ---------------------------------------------------
# XGBOOST
i = 1
results <- list()
for (i in 1:nrow(xgb_grid_1)) {
  print(i)
  print(paste0("Started: ", Sys.time()))

  grid.params <- xgb_grid_1[i, ]
  print(grid.params)

  set.seed(grid.params$seed)
  indexes <- sample(seq_len(nrow(train)), floor(nrow(train)*0.8))
  dtrain <- xgb.DMatrix(data.matrix(train[indexes, feature.names]),
                        label = train[indexes, 'TARGET'])
  dvalid <- xgb.DMatrix(data.matrix(train[-indexes, feature.names]),
                        label = train[-indexes, 'TARGET'])
  watchlist <- list(valid = dvalid, train = dtrain)
  print(paste0("Data created: ", Sys.time()))

  params <- list(booster = "gbtree", objective = "binary:logistic",
                 # scale_pos_weight = 0.1,
                 max_depth = grid.params$max_depth,
                 eta = grid.params$eta,
                 colsample_bytree = grid.params$colsample_bytree,
                 subsample = 0.9)
  set.seed(grid.params$seed)
  model <- xgb.train(params = params, data = dtrain,
                     nrounds = 5000, early.stop.round = 50,
                     eval_metric = 'auc', maximize = T,
                     watchlist = watchlist, print.every.n = 50)
  print(paste0("Model created: ", Sys.time(), ' with AUC: ', model$bestScore))
#   imp <- xgb.importance(sparsed.feature.names, model = model)
#   print(head(imp, 10))

  if (model$bestScore > 0.84) {
    pred <- predict(model, dtest)

    submission <- data.frame(ID = test$ID, TARGET = pred)
    submissionName <- paste0("./results/xg_grid_",
                             format(Sys.time(), "%H_%M_%S"),
                             '_',
                             paste(grid.params, collapse = '_'))
    submissionFile <- paste0(submissionName, ".csv")
    write.csv(submission, submissionFile, row.names=FALSE, quote = FALSE)
    
  }
}

