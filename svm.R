library(e1071)
library(Metrics)

feature.names <- names(train)
feature.names <- feature.names[-match(c('ID', 'TARGET'), feature.names)]
feature.names <- feature.names[-grep('delta', feature.names)]
feature.formula <- formula(paste('TARGET ~ ', paste(feature.names, collapse = ' + '), sep = ''))
feature.formula <- formula(paste('factor(TARGET) ~ ', paste(feature.names, collapse = ' + '), sep = ''))

svm_model <- svm(feature.formula, data=train[indexes, ])
summary(svm_model)

pred <- predict(svm_model, train[-indexes, ])
Metrics::auc(train[-indexes, 'TARGET'], pred)



svm_model <- svm(feature.formula, data=train[1:20000, ], probability=TRUE, cachesize=200)
summary(svm_model)

pred <- predict(svm_model, train[40000:60000, ], probability=TRUE)
pred <- attr(pred, "probabilities")[, 2]
Metrics::rmse(train[40000:60000, 'TARGET'], pred)
View(data.frame(train[40000:60000, 'TARGET'], pred))

pred <- predict(svm_model, test, probability=TRUE)
pred <- attr(pred, "probabilities")[, 2]

winner_11_26$pred <- pred

# svm_tune <- tune(svm, train.x=x, train.y=y, 
#                  kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
# print(svm_tune)

