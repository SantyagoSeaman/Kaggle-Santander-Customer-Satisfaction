

feature.names <- names(train)
feature.names <- feature.names[-match(c('ID', 'TARGET'), feature.names)]
# # feature.names <- feature.names[-match(c('saldo_var24', 'saldo_var42'), feature.names)]
# feature.names <- feature.names[-grep('saldo_medio', feature.names)]
# #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# feature.names <- feature.names[-grep('saldo_var20', feature.names)]
# feature.names <- feature.names[-grep('saldo_var14', feature.names)]
# #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# # feature.names <- feature.names[-20]
feature.formula <- formula(paste('TARGET ~ ', paste(feature.names, collapse = ' + '), sep = ''))


# trainPreproc <- preProcess(train[, feature.names], c('scale', 'center'))
# train2 <- predict(trainPreproc, train[, feature.names])
# train2$ID = train$ID
# train2$TARGET = train$TARGET


# model <- glm(feature.formula, train[1:10000, c(feature.names, "TARGET")], family = binomial())
glm_model <- glm(feature.formula, train[, c(feature.names, "TARGET")], family = binomial())
# summary(glm_model)
lm_model <- lm(feature.formula, train[, c(feature.names, "TARGET")])
# summary(lm_model)

pred <- predict(glm_model, train[, feature.names], type = "response")
zzz <- data.frame(t = train[, 'TARGET'], p = pred)
auc(zzz$t, zzz$p)
View(zzz)
View(zzz[zzz$t == 1, ])
View(zzz[zzz$t == 1 & zzz$p < 0.01, ])
View(zzz[zzz$p < 0.0001, ])
View(zzz[zzz$p > 0.5, ])
View(zzz[zzz$t == 1 & zzz$p < 0.0001, ])

winner.current <- h2o_deeplearning_bag_winner841287_01_44_15
winner.current[, 'glm_pred'] <- predict(glm_model, test[, feature.names], type = "response")
rmse(winner.current$TARGET, winner.current$glm_pred)

View(winner.current[winner.current$glm_pred < 0.0001 & winner.current$TARGET < 0.005, ])
View(winner.current[winner.current$glm_pred < 0.0001 &
                      winner.current$glm_pred > 0.0000000001 & winner.current$TARGET < 0.005, ])

View(winner.current[winner.current$glm_pred > 0.3 & winner.current$TARGET > 0.3, ])

# winner_11_26[, 'glm_pred_comb'] <- apply(winner_11_26, 1, function(row) {
#   if (row['glm_pred'] < 0.0001 & row['TARGET'] < 0.01) return(row['glm_pred'])
#   return(row['TARGET'])
# })

winner.current[, 'glm_pred_comb_filt'] <- apply(winner.current, 1, function(row) {
  # if (row['glm_pred'] < 0.00001 & row['TARGET'] < 0.001) {
  if (row['glm_pred'] < 0.0001 & row['glm_pred'] > 0.0000000001 & row['TARGET'] < 0.005) {
    return(row['TARGET']*0.6 + row['glm_pred']*0.4)
  }
  return(row['TARGET'])
})

winner.current[, 'glm_pred_comb_filt'] <- apply(winner.current, 1, function(row) {
  if (row['glm_pred'] > 0.3 & row['TARGET'] > 0.3) {
    return(max(row['TARGET'], row['glm_pred']))
  }
  return(row['TARGET'])
})

submission <- data.frame(ID = test$ID, TARGET = winner.current[, 'glm_pred_comb_filt'])
submissionName <- paste("./results/glm_combined_filtered_max",
                        format(Sys.time(), "%H_%M_%S"),
                        sep = '_')
submissionFile <- paste0(submissionName, ".csv")
write.csv(submission, submissionFile, row.names=FALSE, quote = FALSE)

