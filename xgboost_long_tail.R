zeroIndex <- c()
zeroIndex <- c(zeroIndex, which(merged$var38 > 4000000))
zeroIndex <- c(zeroIndex, which(merged$saldo_var30 > 950000))
zeroIndex <- c(zeroIndex, which(merged$var15 < 23))
zeroIndex <- c(zeroIndex, which(merged$saldo_medio_var5_hace3 > 200000))
zeroIndex <- c(zeroIndex, which(merged$saldo_medio_var5_hace2 > 170000))
zeroIndex <- c(zeroIndex, which(merged$saldo_medio_var5_ult3 > 120000))
zeroIndex <- c(zeroIndex, which(merged$saldo_medio_var5_ult1 > 88000))
zeroIndex <- c(zeroIndex, which(merged$saldo_medio_var13_largo_ult1 > 0))
zeroIndex <- c(zeroIndex, which(merged$num_meses_var13_largo_ult3 > 0))
zeroIndex <- c(zeroIndex, which(merged$saldo_medio_var13_largo_ult3 > 0))
zeroIndex <- c(zeroIndex, which(merged$saldo_medio_var13_largo_hace2 > 0))
zeroIndex <- c(zeroIndex, which(merged$saldo_var5 > 150000))
zeroIndex <- c(zeroIndex, which(merged$saldo_medio_var13_largo_hace3 > 0))
zeroIndex <- c(zeroIndex, which(merged$ind_var20_0 > 0))
zeroIndex <- c(zeroIndex, which(merged$saldo_var13_largo > 150000))
zeroIndex <- c(zeroIndex, which(merged$saldo_var20 > 0))
zeroIndex <- c(zeroIndex, which(merged$saldo_medio_var8_hace2 > 15000))
zeroIndex <- c(zeroIndex, which(merged$imp_aport_var13_ult1 > 150000))
zeroIndex <- c(zeroIndex, which(merged$saldo_medio_var44_hace2 > 0))
zeroIndex <- c(zeroIndex, which(merged$saldo_var14 > 20000))
zeroIndex <- c(zeroIndex, which(merged$num_var13_largo_0 > 3))
zeroIndex <- c(zeroIndex, which(merged$num_aport_var13_hace3 > 3))
zeroIndex <- c(zeroIndex, which(merged$var3 > 200))
zeroIndex <- c(zeroIndex, which(merged$saldo_medio_var13_corto_hace3 > 160000))
zeroIndex <- c(zeroIndex, which(merged$var21 > 7200))
zeroIndex <- c(zeroIndex, which(merged$saldo_var26 > 10500))
zeroIndex <- c(zeroIndex, which(merged$ind_var33_0 > 0))
zeroIndex <- c(zeroIndex, which(merged$saldo_var42 > 900000))
zeroIndex <- c(zeroIndex, which(merged$imp_trans_var37_ult1 > 500000))
zeroIndex <- c(zeroIndex, which(merged$num_var12 > 5))
zeroIndex <- c(zeroIndex, which(merged$imp_ent_var16_ult1 > 57000))
zeroIndex <- c(zeroIndex, which(merged$num_op_var40_comer_ult3 > 50))
zeroIndex <- c(zeroIndex, which(merged$imp_op_var41_ult1 > 17000))
zeroIndex <- c(zeroIndex, which(merged$imp_op_var39_ult1 > 17000))
zeroIndex <- c(zeroIndex, which(merged$num_venta_var44_ult1 > 0))
zeroIndex <- c(zeroIndex, which(merged$num_aport_var17_hace3 > 0))
zeroIndex <- c(zeroIndex, which(merged$saldo_medio_var8_hace3 > 1500))
zeroIndex <- c(zeroIndex, which(merged$num_op_var39_ult3 > 220))
zeroIndex <- c(zeroIndex, which(merged$num_op_var41_hace3 > 30))
zeroIndex <- c(zeroIndex, which(merged$saldo_medio_var12_hace2 > 700000))
zeroIndex <- c(zeroIndex, which(merged$num_compra_var44_ult1 > 3))
zeroIndex <- c(zeroIndex, which(merged$num_op_var39_comer_ult1 > 130))
zeroIndex <- c(zeroIndex, which(merged$num_op_var39_comer_ult3 > 250))
zeroIndex <- c(zeroIndex, which(merged$saldo_medio_var13_corto_ult3 > 350000))
zeroIndex <- c(zeroIndex, which(merged$imp_var7_recib_ult1 > 85000))
zeroIndex <- c(zeroIndex, which(merged$num_var43_recib_ult1 > 60))
zeroIndex <- c(zeroIndex, which(merged$num_var13_corto_0 > 3))
zeroIndex <- c(zeroIndex, which(merged$num_trasp_var11_ult1 > 30))
zeroIndex <- c(zeroIndex, which(merged$saldo_var30 > 12000 & merged$var15 > 28))

zeroIndex <- unique(zeroIndex)

merged.zero <- merged
merged.zero$is_zero <- 0
merged.zero[zeroIndex, 'is_zero'] <- 1

# ------------------------------------------------------------
# noise
# indexes <- unique(which(merged.zero$saldo_var30 > 90 & merged.zero$var15 < 25 & merged.zero$count0 > 142),
#                   which(merged.zero$saldo_var30 > 20 & merged.zero$count0 == 146 & merged.zero$var15 < 24))
# merged.zero[indexes, 'is_zero'] <- 1

# ------------------------------------------------------------
train.zero <- merged.zero[merged.zero$TARGET != -1, ]
test.zero <- merged.zero[merged.zero$TARGET == -1, ]

# train <- train.zero
# test <- test.zero

# ------------------------------------------------------------
# ------------------------------------------------------------
feature.names <- names(train.zero)
feature.names <- feature.names[-match(c('ID', 'TARGET'), feature.names)]
feature.formula <- formula(paste('TARGET ~ ', paste(feature.names, collapse = ' + '), sep = ''))

dfull <- xgb.DMatrix(data.matrix(train.zero[, feature.names]), label = train.zero$TARGET)
dtest <- data.matrix(test.zero[, feature.names])

xgb_params <- list(booster = "gbtree", objective = "binary:logistic",
                   # eval_metric = "rmse", maximize = F,
                   eval_metric = "auc", maximize = T,
                   max_depth = 5,
                   eta = 0.005,
                   colsample_bytree = 0.5,
                   subsample = 0.7)

seed.number <- 100000
set.seed(seed.number)
xgb_cv = xgb.cv(params = xgb_params,
                data = dfull,
                nrounds = 20000,
                nfold = 7,
                prediction = F,
                showsd = F,
                stratified = F,
                verbose = T,
                print.every.n = 100,
                early.stop.round = ceiling(log(1/grid.params$eta)*30))

max.auc <- max(xgb_cv$test.auc.mean)
max.auc.step <- which.max(xgb_cv$test.auc.mean)



seed.number <- 100006
set.seed(seed.number)
indexes <- sample(seq_len(nrow(train.zero)), floor(nrow(train.zero)*0.7))
dtrain <- xgb.DMatrix(data.matrix(train.zero[indexes, feature.names]),
                      label = train.zero[indexes, 'TARGET'])
dvalid <- xgb.DMatrix(data.matrix(train.zero[-indexes, feature.names]),
                      label = train.zero[-indexes, 'TARGET'])
watchlist <- list(valid = dvalid, train = dtrain)

set.seed(seed.number)
model <- xgb.train(params = xgb_params, data = dtrain,
                   nrounds = 2000,
                   early.stop.round = 150,
                   watchlist = watchlist,
                   print.every.n = 100)
# imp <- xgb.importance(model = model, feature_names = feature.names)
pred.valid <- predict(model, dvalid)
auc(train.zero[-indexes, 'TARGET'], pred.valid)

valid.zeros <- rep(0, as.integer((nrow(train) - nrow(train.zero)) * (1-sample.part)))
auc(c(train.zero[-indexes, 'TARGET'], valid.zeros), c(pred.valid, valid.zeros))

pred.full <- predict(model, dfull)
auc(train.zero$TARGET, pred.full)

pred.test <- predict(model, dtest)
View(data.frame(w = current.winner.pred, p = pred.test))




# ------------------------------------------------------------
# ------------------------------------------------------------
train.zero.factor <- train.zero
train.zero.factor$TARGET <- factor(train.zero.factor$TARGET)
indexes <- sample(seq_len(nrow(train.zero.factor)), floor(nrow(train.zero.factor)*0.8))
trainHex <- as.h2o(train.zero.factor[indexes, c(feature.names, 'TARGET')], destination_frame="train.hex")
#summary(trainHex)
validHex <- as.h2o(train.zero.factor[-indexes, c(feature.names, 'TARGET')], destination_frame="valid.hex")
testHex <- as.h2o(test[, feature.names], destination_frame="test.hex")
fullHex <- as.h2o(train.zero.factor[, c(feature.names, 'TARGET')], destination_frame="full.hex")

for (n in seq(50, 200, 50)) {
  print(n)
  gbmHex <- h2o.gbm(x = feature.names,
                    y = "TARGET",
                    training_frame = trainHex,
                    validation_frame = validHex,
                    model_id = "gbmStarter.hex",
                    distribution = "AUTO",
#                     nfolds = 7,
#                     stopping_rounds = 10,
#                     stopping_metric = 'AUC',
#                     stopping_tolerance = 0.01,
                    seed = 123 + n,
                    ntrees = n,
                    max_depth = 5,
                    # min_rows = 10,
                    learn_rate = 0.05,
                    col_sample_rate = 0.5,
                    sample_rate = 1,
                    # keep_cross_validation_predictions = F,
                    balance_classes = T)
  # print(gbmHex)
  print(gbmHex@model$validation_metrics)
}


print(summary(gbmHex))
# head(gbmHex@model$variable_importances, 20)
predictionsList <- c(predictionsList, h2o.predict(gbmHex, testHex))
predictionsListFull <- c(predictionsListFull, h2o.predict(gbmHex, fullHex))

