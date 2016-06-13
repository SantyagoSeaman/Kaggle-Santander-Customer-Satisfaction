library(xgboost)
library(Matrix)

set.seed(1234)

train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

##### Removing IDs
train$ID <- NULL
test.id <- test$ID
test$ID <- NULL

##### Extracting TARGET
train.y <- train$TARGET
train$TARGET <- NULL

var15 = test['var15']
saldo_medio_var5_hace2 = test['saldo_medio_var5_hace2']
saldo_var33 = test['saldo_var33']
var38 = test['var38']
V21 = test['var21']
NV=test['num_var33']+test['saldo_medio_var33_ult3']+test['saldo_medio_var44_hace2']+test['saldo_medio_var44_hace3']+
test['saldo_medio_var33_ult1']+test['saldo_medio_var44_ult1']


##### 0 count per line
count0 <- function(x) {
  return( sum(x == 0) )
}
train$n0 <- apply(train, 1, FUN=count0)
test$n0 <- apply(test, 1, FUN=count0)

##### Removing constant features
cat("\n## Removing the constants features.\n")
for (f in names(train)) {
  if (length(unique(train[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.\n")
    train[[f]] <- NULL
    test[[f]] <- NULL
  }
}

##### Removing identical features
features_pair <- combn(names(train), 2, simplify = F)
toRemove <- c()
for(pair in features_pair) {
  f1 <- pair[1]
  f2 <- pair[2]
  
  if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
    if (all(train[[f1]] == train[[f2]])) {
      cat(f1, "and", f2, "are equals.\n")
      toRemove <- c(toRemove, f2)
    }
  }
}

feature.names <- setdiff(names(train), toRemove)

train$var38 <- log(train$var38)
test$var38 <- log(test$var38)

train <- train[, feature.names]
test <- test[, feature.names]

#---limit vars in test based on min and max vals of train
print('Setting min-max lims on test data')
for(f in colnames(train)){
  lim <- min(train[,f])
  test[test[,f]<lim,f] <- lim
  
  lim <- max(train[,f])
  test[test[,f]>lim,f] <- lim  
}
#---

train$TARGET <- train.y


train <- sparse.model.matrix(TARGET ~ ., data = train)

dtrain <- xgb.DMatrix(data=train, label=train.y)
watchlist <- list(train=dtrain)

param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "auc",
                eta                 = 0.0202048,
                max_depth           = 5,
                subsample           = 0.6815,
                colsample_bytree    = 0.701
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 560, 
                    verbose             = 1,
                    watchlist           = watchlist,
                    maximize            = FALSE
)


#######actual variables

feature.names

test$TARGET <- -1

test <- sparse.model.matrix(TARGET ~ ., data = test)

preds <- predict(clf, test)
pred <-predict(clf,train)
AUC<-function(actual,predicted)
{
  library(pROC)
  auc<-auc(as.numeric(actual),as.numeric(predicted))
  auc 
}
AUC(train.y,pred) ##AUC
# Under 23 year olds are always happy
preds[var15 < 23] = 0
preds[saldo_medio_var5_hace2 > 160000]=0
preds[saldo_var33 > 0]=0
preds[var38 > 3988596]=0
preds[NV>0]=0
preds[V21>7500]=0

submission <- data.frame(ID=test.id, TARGET=preds)
cat("saving the submission file\n")
write.csv(submission, "submission.csv", row.names = F)