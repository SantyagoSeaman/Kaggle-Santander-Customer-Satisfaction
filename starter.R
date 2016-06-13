library(xgboost)
library(Matrix)
library(caret)
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
# Convert
nearZero <- nearZeroVar(merged, saveMetrics = T, freqCut = 150000/30, uniqueCut = 10/150000 * 100)
nearZeroRowNames <- rownames(nearZero[nearZero$nzv == T, ])

feature.train.names <- names(orig.train)
feature.train.names <- feature.train.names[!(feature.train.names %in% nearZeroRowNames)]
merged <- merged[, feature.train.names]


feature.train.names <- names(merged)[-1]
feature.train.names <- feature.train.names[-length(feature.train.names)]
for (f in feature.train.names) {
#   if (class(merged[[f]]) == "numeric") {
#     merged[[f]] <- merged[[f]] / max(merged[[f]])
#   }
  if (length(unique(merged[[f]])) < 10) {
    merged[[paste0(f, '_factor')]] <- factor(merged[[f]])
  }
}

merged$var15_factor <- factor(merged$var15)
merged$var15 <- NULL
# merged$var38 <- log(merged$var38 + 1)
# merged$saldo_var30 <- sapply(merged$saldo_var30, get.log)
# merged$saldo_medio_var5_hace3 <- sapply(merged$saldo_medio_var5_hace3, get.log)
# merged$saldo_medio_var5_hace2 <- sapply(merged$saldo_medio_var5_hace2, get.log)
# merged$saldo_medio_var5_ult3 <- sapply(merged$saldo_medio_var5_ult3, get.log)
# merged$saldo_medio_var5_ult1 <- sapply(merged$saldo_medio_var5_ult1, get.log)
# merged$num_var45_hace3 <- sapply(merged$num_var45_hace3, get.log)
# merged$num_var45_ult3 <- sapply(merged$num_var45_ult3, get.log)
# merged$num_var45_hace2 <- sapply(merged$num_var45_hace2, get.log)


# ---------------------------------------------------
# Calc deltas
feature.names <- names(merged)
saldo.vars <- feature.names[grep('saldo_', feature.names)]
# saldo.vars
# View(train[, c('ID', 'TARGET', saldo.vars)])

for(fn1.index in 1:(length(saldo.vars)-1)) {
  for(fn2.index in (fn1.index+1):length(saldo.vars)) {
    fn1.name <- saldo.vars[fn1.index]
    fn2.name <- saldo.vars[fn2.index]
    merged[[paste('sld', fn1.name, fn2.name, sep = '_')]] <- merged[[fn1.name]] - merged[[fn2.name]]
  }
}


# ---------------------------------------------------
# Split
train <- merged[merged$TARGET != -1, ]
test <- merged[merged$TARGET == -1, ]


# ---------------------------------------------------
# Features
feature.names <- names(train)
feature.names <- feature.names[-grep('^ID$', feature.names)]
feature.names <- feature.names[-grep('^TARGET$', feature.names)]
feature.formula <- formula(paste('TARGET ~ ', paste(feature.names, collapse = ' + '), sep = ''))

# dfull <- xgb.DMatrix(sparse.model.matrix(feature.formula, data = train), label = train$TARGET)
dtest <- sparse.model.matrix(feature.formula, data = test)
sparsed.feature.names <- colnames(dtest)

# ---------------------------------------------------
# XGBOOST
i = 1
results <- list()
for (i in 1:1000) {
  print(i)
  print(paste0("Started: ", Sys.time()))

  set.seed(10 + i)
  indexes <- sample(seq_len(nrow(train)), floor(nrow(train)*0.8))

  dtrain <- xgb.DMatrix(sparse.model.matrix(feature.formula, data = train[indexes, ]),
                        label = train[indexes, 'TARGET'])
  dvalid <- xgb.DMatrix(sparse.model.matrix(feature.formula, data = train[-indexes, ]),
                        label = train[-indexes, 'TARGET'])
  watchlist <- list(valid = dvalid, train = dtrain)
  print(paste0("Data created: ", Sys.time()))

  rand.max.depth <- round(runif(1, 6, 10), 0)
  rand.eta <- round(runif(1, 0.01, 0.1), 3)
  print(paste0("Max depth: ", rand.max.depth, ', eta: ', rand.eta))

  params <- list(booster = "gbtree", objective = "binary:logistic",
                 max_depth = rand.max.depth, eta = rand.eta,
                 colsample_bytree = 0.7, subsample = 0.9)
#   params <- list(booster = "gbtree", objective = "binary:logistic",
#                  max_depth = 6, eta = 0.01,
#                  colsample_bytree = 0.7, subsample = 0.9)
  model <- xgb.train(params = params, data = dtrain,
                     nrounds = 1000, early.stop.round = 100,
                     eval_metric = 'auc', maximize = T,
                     watchlist = watchlist, print.every.n = 20)
  print(paste0("Model created: ", Sys.time()))
  # imp <- xgb.importance(sparsed.feature.names, model = model)

  if (model$bestScore > 0.84) {
    pred <- predict(model, dtest)
    results <- c(results, list(pred))
    print(paste0("Predicted: ", Sys.time()))
  }
  # pred <- predict(model, dfull)
}

results.mean <- unlist(c(results[1]))
for(index in 2:length(results)) {
  results.mean <- cbind(results.mean, unlist(c(results[index])))
}
results.mean <- rowMeans(results.mean)

# ---------------------------------------------------
# SAVE
submission <- data.frame(ID = test$ID, TARGET = results.mean)
# submission <- data.frame(ID = test$ID, TARGET = pred)
submissionName <- paste0("./results/xg_bag_", format(Sys.time(), "%H_%M_%S"), '_rand_', length(results))
submissionFile <- paste0(submissionName, ".csv")
write.csv(submission, submissionFile, row.names=FALSE, quote = FALSE)

