zzz <- train
zzz$pred <- pred

View(zzz[zzz$TARGET == 1 & zzz$pred < 0.01, c(imp[1:20, ]$Feature, 'TARGET', 'pred')])

View(zzz[zzz$TARGET == 1 & zzz$saldo_var30 == 3, c(imp[1:50, ]$Feature, 'TARGET', 'pred')])

table(zzz[zzz$saldo_var30 > 1000, 'TARGET'])
table(zzz[zzz$saldo_var30 == 3 & zzz$var38 == -1 & zzz$var15 > 25, 'TARGET'])
table(zzz[zzz$saldo_var30 == 3 & zzz$var38 == -1 & zzz$var15 > 29, 'TARGET'])
View(zzz[zzz$saldo_var30 == 3 & zzz$var38 == -1 & zzz$var15 > 29, c(imp[1:20, ]$Feature, 'TARGET', 'pred')])

table(zzz[zzz$saldo_var30 == 3 & zzz$var38 == -1 & zzz$var15 > 30 & zzz$count0 %in% 144:146, 'TARGET'])
View(zzz[zzz$saldo_var30 == 3 & zzz$var38 == -1 & zzz$var15 > 30 & zzz$count0 %in% 144:146, c('TARGET', 'pred', imp[1:20, ]$Feature)])


table(zzz[zzz$saldo_var30 == 3 & zzz$var38 == -1 & zzz$var15 > 30 & zzz$count0 %in% 144:146 & zzz$num_var45_hace3 == 0 & zzz$num_meses_var39_vig_ult3 == 2, c('TARGET')])
View(zzz[zzz$saldo_var30 == 3 & zzz$var38 == -1 & zzz$var15 > 30 & zzz$count0 %in% 144:146 & zzz$num_var45_hace3 == 0 & zzz$num_meses_var39_vig_ult3 == 2,
         c(imp[1:70, ]$Feature, 'TARGET', 'pred')])
View(zzz[zzz$saldo_var30 == 3 & zzz$var38 == -1 & zzz$var15 > 30 & zzz$count0 %in% 144:146 & zzz$num_var45_hace3 == 0 & zzz$num_meses_var39_vig_ult3 == 2,
         c(imp$Feature, 'TARGET', 'pred')])
write.csv(zzz[zzz$saldo_var30 == 3 & zzz$var38 == -1 & zzz$var15 > 30 & zzz$count0 %in% 144:146 & zzz$num_var45_hace3 == 0 & zzz$num_meses_var39_vig_ult3 == 2,
              ], 'aaa.csv')



# -------------------------------------------------------------------------------------------------------
table(zzz[zzz$count0 > 158, 'TARGET'])
table(zzz[zzz$saldo_var30 > 100, 'var15'])
table(zzz[zzz$saldo_var30 > 100 & zzz$var15 < 25, 'TARGET']) # 2895:12
nrow(test[test$saldo_var30 > 100 & test$var15 < 25, ]) # 2860
table(zzz[zzz$saldo_var30 > 10000 & zzz$var15 < 25, 'TARGET']) # 145:1

table(zzz[zzz$saldo_var30 > 90 & zzz$var15 < 25 & zzz$count0 > 142, 'TARGET']) # 1269:0 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
nrow(test[test$saldo_var30 > 90 & test$var15 < 25 & test$count0 > 142, ]) # 1158

table(zzz[zzz$saldo_var30 > 100 & zzz$var15 < 25 & zzz$count0 > 142, 'TARGET'])


table(zzz[zzz$saldo_var30 > 20000 & zzz$var15 < 28, 'TARGET']) # 109
table(zzz[zzz$saldo_var30 > 12000 & zzz$var15 < 28, 'TARGET']) # 163
# table(zzz[zzz$saldo_var30 > 400000 & zzz$var15 > 50, 'TARGET'])
table(zzz[zzz$saldo_var30 > 100000 & zzz$var15 > 65, 'TARGET']) # 654
table(zzz[zzz$saldo_var30 > 200000 & zzz$var15 > 70, 'TARGET']) # 270
table(zzz[zzz$saldo_var30 > 500000 & zzz$var15 > 40, 'TARGET']) # 140
table(zzz[zzz$saldo_var30 > 400000 & zzz$var15 > 50, 'TARGET']) # 227
table(zzz[zzz$saldo_var30 > 400000 & zzz$var38 > 100000 & zzz$var15 > 60, 'TARGET']) # 98
table(zzz[zzz$saldo_var30 > 400000 & zzz$var38 > 100000 & zzz$var15 > 50 & zzz$var15 < 60, 'TARGET']) # 98
table(zzz[zzz$saldo_var30 > 500000 & zzz$var15 > 30 & zzz$var15 < 60, 'TARGET'])
table(zzz[zzz$var38 < 100000 & zzz$saldo_var30 < 10 & zzz$var15 > 50, 'TARGET'])
table(zzz[zzz$var38 < 90000 & zzz$saldo_var30 < 10 & zzz$var15 > 50, 'TARGET'])
table(zzz[zzz$var15 < 30, 'TARGET'])

View(zzz[zzz$saldo_var30 > 10000 & zzz$var15 < 30, c('TARGET', 'pred', imp[1:10, ]$Feature)])

# --------------------------
# пример недовольных клиентов
table(zzz[zzz$var38 < 100000 & zzz$saldo_var30 < 10 & zzz$var15 > 50, c('TARGET')])
table(zzz[zzz$var38 < 100000 & zzz$saldo_var30 < 10 & zzz$var15 > 50 & zzz$pred > 0.2, c('TARGET')])
table(zzz[zzz$var38 < 100000 & zzz$saldo_var30 < 10 & zzz$var15 > 50 & zzz$saldo_var30 < 0, c('TARGET')])
View(zzz[zzz$var38 < 100000 & zzz$saldo_var30 < 10 & zzz$var15 > 50, c('TARGET', 'pred', imp[1:15, ]$Feature)])
View(zzz[zzz$var38 < 100000 & zzz$saldo_var30 < 10 & zzz$var15 > 50 & zzz$pred > 0.2, c('TARGET', 'pred', imp[1:15, ]$Feature)])
# --------------------------



results.mean.final[test$saldo_var30 > 100 & test$var15 < 25 & test$count0 > 142] <- 0
results.mean.final[test$saldo_var30 > 100 & test$var15 < 25 & test$count0 > 142 & results.mean.final > 0.05]
results.mean.final[test$saldo_var30 > 100 & test$var15 < 25 & test$count0 > 142]
View(test[test$saldo_var30 > 100 & test$var15 < 25 & test$count0 > 142, c('saldo_var30', 'var15', 'count0')])
View(zzz[zzz$saldo_var30 > 100 & zzz$var15 < 25 & zzz$count0 > 142, c('saldo_var30', 'var15', 'count0', 'pred')])
length(results.mean.final[results.mean.final == 0])


table(train[train$count0 == 146, 'TARGET'])
table(train[train$saldo_var30 > 10000 & train$count0 == 146, 'TARGET']) # 82
table(train[train$saldo_var30 > 20 & train$count0 == 146 & zzz$var15 < 24, 'TARGET']) # 1553:0 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

table(train[train$saldo_var30 < 10 & train$count0 == 156 & zzz$var15 > 40 & zzz$num_var45_hace3 == 0 & zzz$var38 == -1, 'TARGET'])
table(train[train$count0 == 156 & zzz$var15 < 24 & zzz$num_var45_hace3 == 0, 'TARGET'])
View(zzz[zzz$saldo_var30 > 10 & zzz$count0 == 146 & zzz$TARGET == 1,
         c('TARGET', 'pred', imp[1:30, ]$Feature)])
View(zzz[zzz$saldo_var30 > 10 & zzz$count0 == 146,
         c('TARGET', 'pred', imp[1:30, ]$Feature)])

write.csv(zzz[zzz$saldo_var30 > 10 & zzz$count0 == 146, c('TARGET', 'pred', imp[1:30, ]$Feature)], 'aaa.csv')

# ------------------------------------------------------------------------------------------
ggplot(data=train, aes(factor(count0))) +
  geom_histogram(alpha = .5, col = 'black') +
  facet_grid(~ TARGET, scales = 'free_y', space = 'free', drop = F)
ggplot(data=train[train$TARGET==1 & train$count0 != 355, ], aes(factor(count0))) +
  geom_histogram(alpha = .5, col = 'black')
table(train[train$TARGET == 1, 'count0'])
table(train[train$TARGET == 0, 'count0'])

# -------------------------------------------------------------------------------------------------------
table(train[train$count0 == 158, 'TARGET'])
table(train[train$count0 == 158 & zzz$var15 > 30 & zzz$var38 > 10, 'TARGET'])
View(zzz[zzz$count0 == 158 & zzz$var15 > 30 & zzz$var38 > 10,
         c('TARGET', 'pred', imp[1:30, ]$Feature)])
View(zzz[zzz$count0 == 154 & zzz$TARGET == 1,
         c('TARGET', 'pred', imp[1:30, ]$Feature)])


View(zzz[zzz$TARGET == 1 & zzz$pred < 0.01,
         c('TARGET', 'pred', imp[1:30, ]$Feature)])

# -------------------------------------------------------------------------------------------------------
zzz$pred_2 <- zzz$pred
auc(zzz$TARGET, zzz$pred_2)

zzz[train.zero$is_zero == 1, 'pred_2'] <- 0

indexes <- which(zzz$saldo_var30 > 90 & zzz$var15 < 25 & zzz$count0 > 142)
indexes <- c(indexes, which(zzz$saldo_var30 > 20 & zzz$count0 == 146 & zzz$var15 < 24))
indexes <- unique(indexes)
zzz[indexes, 'pred_2'] <- 0

length(zzz[zzz$pred_2 == 0, 'pred_2'])
zzz$pred_2



winner <- winner_long_tail_122043_num_var43_recib_ult1_60$TARGET

indexes <- which(test$saldo_var30 > 100 & test$var15 < 25 & test$count0 > 142)
indexes <- c(indexes, which(test$saldo_var30 > 20 & test$count0 == 146 & test$var15 < 24))
indexes <- unique(indexes)

winner[indexes] <- 0
length(winner[winner==0])
save.submission(test$ID, winner, 'results', 'winner_long_tail', c('wo_noise', 'saldo_var30', 100, 'var15', 25, 'count0', 142))


