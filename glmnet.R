library(glmnet)
require(doMC)
registerDoMC(cores=2)


# ==================================================================================
glm_model <- glmnet(x = as.matrix(train[1:1000, feature.names]),
                    y = as.factor(train[1:1000, 'TARGET']),
                    family = 'binomial')

head(as.matrix(coef(glm_model)))
glm_model

# pred <- predict(fit, newx = train[10000:11000, feature.names], type = "class", s = c(0.05, 0.01))
pred <- predict(glm_model, newx = as.matrix(train[10000:11000, feature.names]), type = "class", s = c(0.1, 0.5))
pred <- predict(glm_model, newx = as.matrix(train[10000:11000, feature.names]), type = "class")


# ==================================================================================
set.seed(10)
indexes <- sample(seq_len(nrow(train)), floor(nrow(train)*0.8))
cvfit = cv.glmnet(x = as.matrix(train[indexes, feature.names]),
                  y = as.factor(train[indexes, 'TARGET']),
                  family = 'binomial', type.measure = "auc",
                  nfolds = 5, parallel = T)

# plot(cvfit)
cvfit$lambda.min
head(coef(cvfit, s = "lambda.min"), 20)

pred <- predict(cvfit, newx = as.matrix(train[-indexes, feature.names]), type = "response", s = "lambda.min")
# pred <- cbind(pred, train[10000:20000, 'TARGET'])
Metrics::auc(train[-indexes, 'TARGET'], pred)

# 0.8088535


