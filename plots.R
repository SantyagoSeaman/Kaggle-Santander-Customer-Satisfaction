Ilibrary(ggplot2)

View(train[, c('TARGET', head(imp$Feature, 10))])


hist(train[train$TARGET == 1, 'var15'])
hist(train[train$TARGET == 0, 'var15'])
# table(train[train$var15 > 95, 'TARGET'])
table(train[train$var15 < 23, 'TARGET'])

# Начинать от 1000000: +0.000003
# проверить 600000-1000000
hist(train[train$TARGET == 1, 'saldo_var30'])
hist(train[train$TARGET == 0, 'saldo_var30'])
table(train[train$saldo_var30 > 550000, 'TARGET'])

# 4000000: +0.000044
# Сильная фича
hist(train[train$TARGET == 1, 'var38'])
hist(train[train$TARGET == 0, 'var38'])
table(train[train$var38 > 4000000, 'TARGET'])

# Странная фича. Хороший результат на 20К, но с одним позитивным результатом.
hist(train[train$TARGET == 1, 'saldo_medio_var5_hace3'])
hist(train[train$TARGET == 0, 'saldo_medio_var5_hace3'])
table(train[train$saldo_medio_var5_hace3 > 200000, 'TARGET'])

# 170000: +0.000038
hist(train[train$TARGET == 1, 'saldo_medio_var5_hace2'])
hist(train[train$TARGET == 0, 'saldo_medio_var5_hace2'])
table(train[train$saldo_medio_var5_hace2 > 170000, 'TARGET'])

hist(train[train$TARGET == 1, 'saldo_medio_var5_ult3'])
hist(train[train$TARGET == 0, 'saldo_medio_var5_ult3'])
table(train[train$saldo_medio_var5_ult3 > 120000, 'TARGET'])

hist(train[train$TARGET == 1, 'num_var4'])
hist(train[train$TARGET == 0, 'num_var4'])
table(train[train$num_var4 > 6, 'TARGET'])

# hist(train[train$TARGET == 1, 'num_var35'])
# hist(train[train$TARGET == 0, 'num_var35'])
# table(train[train$num_var35 > 21, 'TARGET'])

# hist(train[train$TARGET == 1, 'num_var22_ult3'])
# hist(train[train$TARGET == 0, 'num_var22_ult3'])
# table(train[train$num_var22_ult3 > 100, 'TARGET'])

hist(train[train$TARGET == 1, 'saldo_medio_var5_ult1'])
hist(train[train$TARGET == 0, 'saldo_medio_var5_ult1'])
table(train[train$saldo_medio_var5_ult1 > 90000, 'TARGET'])

hist(train[train$TARGET == 1, 'saldo_var5'])
hist(train[train$TARGET == 0, 'saldo_var5'])
table(train[train$saldo_var5 > 150000, 'TARGET'])

hist(train[train$TARGET == 1, 'saldo_var8'])
hist(train[train$TARGET == 0, 'saldo_var8'])
table(train[train$saldo_var8 > 70000, 'TARGET'])

hist(train[train$TARGET == 1, 'saldo_medio_var13_largo_ult1'])
hist(train[train$TARGET == 0, 'saldo_medio_var13_largo_ult1'])
table(train[train$saldo_medio_var13_largo_ult1 > 0, 'TARGET'])
table(train[train$saldo_medio_var13_largo_ult1 == 0, 'TARGET'])



# ------------------------------------------------------------------------------------------
ggplot(data=train, aes(factor(var15))) +
  geom_histogram(alpha = .2, col='red', fill="red") +
  geom_histogram(data = test, alpha = .2, col="blue", fill="blue")

ggplot(data=train[train$TARGET == 0, ], aes(var15)) +
  geom_histogram(binwidth = 1, alpha = .2, col='red', fill="red")

ggplot(train, aes(var15, fill = factor(TARGET), col = factor(TARGET))) +
  geom_density(binwidth = 1, alpha = 0.5) +
  geom_density(data = test, binwidth = 1, alpha = 0.5)

ggplot(data=train, aes(factor(var15), fill = factor(TARGET), col = factor(TARGET))) +
  geom_histogram(alpha = .2, col='red', fill="red") +
  geom_histogram(data = test, alpha = .2, col="blue", fill="blue")


ggplot(data=train[indexes, ], aes(factor(var15))) +
  geom_histogram(alpha = .2, col='red', fill="red") +
  geom_histogram(data = train[-indexes, ], alpha = .2, col="blue", fill="blue")
ggplot(train[indexes, ], aes(var15, fill = factor(TARGET), col = factor(TARGET))) +
  geom_density(binwidth = 1, alpha = 0.5) +
  geom_density(data = train[-indexes, ], binwidth = 1, alpha = 0.5, col='black')

# ------------------------------------------------------------------------------------------
table(round(train[train$TARGET == 0 & train$saldo_var30 < 100, 'saldo_var30']))
table(round(train[train$TARGET == 1 & train$saldo_var30 < 100, 'saldo_var30']))

ggplot(data=train[train$saldo_var30 != 0 & train$saldo_var30 < 100000, ], aes(saldo_var30)) +
  geom_histogram(binwidth = 1000, alpha = .2, col='black')
ggplot(data=train[train$TARGET == 0 & train$saldo_var30 != 0 & train$saldo_var30 < 100000, ], aes(saldo_var30)) +
  geom_histogram(binwidth = 1000, alpha = .2, col='black')
ggplot(data=train[train$TARGET == 1 & train$saldo_var30 != 0 & train$saldo_var30 < 100000, ], aes(saldo_var30)) +
  geom_histogram(binwidth = 1000, alpha = .2, col='black')
ggplot(data=test[test$saldo_var30 != 0 & test$saldo_var30 < 100000, ], aes(saldo_var30)) +
  geom_histogram(binwidth = 1000, alpha = .2, col="black")

ggplot(train[train$saldo_var30 != 0 & train$saldo_var30 > -500 & train$saldo_var30 < 1000, ],
       aes(saldo_var30, fill = factor(TARGET), col = factor(TARGET))) +
  geom_density(alpha = 0.5) +
  geom_density(data = test[test$saldo_var30 != 0 & test$saldo_var30 > -500 & test$saldo_var30 < 1000, ],
               alpha = 0.5)

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Проверить var38!!!!!!!!!!!
table(round(train[train$TARGET == 0 & train$var38 < 10000, 'var38']))
table(round(train[train$TARGET == 1 & train$var38 < 10000, 'var38']))
ggplot(train[train$var38 != -1 ? train$var38 < 1000000, ],
       aes(var38, fill = factor(TARGET), col = factor(TARGET))) +
  geom_density(alpha = 0.5) +
  geom_density(data = test[test$var38 != -1 & test$var38 < 1000000, ],
               alpha = 0.5)

# ------------------------------------------------------------------------------------------
table(round(train[train$TARGET == 0 & train$saldo_medio_var5_hace3 < 100, 'saldo_medio_var5_hace3']))

ggplot(train[train$saldo_medio_var5_hace3 != 0 & train$saldo_medio_var5_hace3 < 100, ],
       aes(saldo_medio_var5_hace3, fill = factor(TARGET), col = factor(TARGET))) +
  geom_density(alpha = 0.5) +
  geom_density(data = test[test$saldo_medio_var5_hace3 != 0 & test$saldo_medio_var5_hace3 < 100, ], alpha = 0.5)

# ------------------------------------------------------------------------------------------

ggplot(train[train$saldo_medio_var5_hace2 != 0 & train$saldo_medio_var5_hace2 > -100 & train$saldo_medio_var5_hace2 < 100, ],
       aes(saldo_medio_var5_hace2, fill = factor(TARGET), col = factor(TARGET))) +
  geom_density(alpha = 0.5) +
  geom_density(data = test[test$saldo_medio_var5_hace2 != 0 & test$saldo_medio_var5_hace2 > -100 & test$saldo_medio_var5_hace2 < 100, ], alpha = 0.5)

# ------------------------------------------------------------------------------------------

ggplot(train[train$saldo_medio_var5_ult3 != 0 & train$saldo_medio_var5_ult3 > -100 & train$saldo_medio_var5_ult3 < 100, ],
       aes(saldo_medio_var5_ult3, fill = factor(TARGET), col = factor(TARGET))) +
  geom_density(alpha = 0.5) +
  geom_density(data = test[test$saldo_medio_var5_ult3 != 0 & test$saldo_medio_var5_ult3 > -100 & test$saldo_medio_var5_ult3 < 100, ], alpha = 0.5)

# ------------------------------------------------------------------------------------------
ggplot(data=train, aes(factor(num_var4), fill = factor(TARGET), col = factor(TARGET))) +
  geom_histogram(alpha = .2) +
  geom_histogram(data = test, alpha = .2)

ggplot(train, aes(factor(num_var4), fill = factor(TARGET), col = factor(TARGET))) +
  geom_density(binwidth = 1, alpha = 0.5) +
  geom_density(data = test, binwidth = 1, alpha = 0.5)

# ------------------------------------------------------------------------------------------
ggplot(data=train, aes(factor(num_var35), fill = factor(TARGET), col = factor(TARGET))) +
  geom_histogram(alpha = .2) +
  geom_histogram(data = test, alpha = .2)


# ------------------------------------------------------------------------------------------
ggplot(data=train, aes(factor(num_var22_ult3), fill = factor(TARGET), col = factor(TARGET))) +
  geom_histogram(alpha = .2) +
  geom_histogram(data = test, alpha = .2)

# ------------------------------------------------------------------------------------------
ggplot(data=train, aes(factor(ind_var30), fill = factor(TARGET), col = factor(TARGET))) +
  geom_histogram(alpha = .2) +
  geom_histogram(data = test, alpha = .2)

# ------------------------------------------------------------------------------------------
ggplot(train[train$saldo_medio_var5_ult1 != 0 & train$saldo_medio_var5_ult1 > -100 & train$saldo_medio_var5_ult1 < 200, ],
       aes(saldo_medio_var5_ult1, fill = factor(TARGET), col = factor(TARGET))) +
  geom_density(alpha = 0.5) +
  geom_density(data = test[test$saldo_medio_var5_ult1 != 0 & test$saldo_medio_var5_ult1 > -100 & test$saldo_medio_var5_ult1 < 200, ], alpha = 0.5)


# ------------------------------------------------------------------------------------------
ggplot(train[train$saldo_medio_var13_largo_hace3 != 0, ],
       aes(saldo_medio_var13_largo_hace3, fill = factor(TARGET), col = factor(TARGET))) +
  geom_density(alpha = 0.5) +
  geom_density(data = test[test$saldo_medio_var13_largo_hace3 != 0, ], alpha = 0.5)

ggplot(train[train$saldo_medio_var13_largo_hace3 != 0 & train$saldo_medio_var13_largo_hace3 > -100 & train$saldo_medio_var13_largo_hace3 < 100, ],
       aes(saldo_medio_var13_largo_hace3, fill = factor(TARGET), col = factor(TARGET))) +
  geom_density(alpha = 0.5) +
  geom_density(data = test[test$saldo_medio_var13_largo_hace3 != 0 & test$saldo_medio_var13_largo_hace3 > -100 & test$saldo_medio_var13_largo_hace3 < 100, ], alpha = 0.5)




# ggplot(data=train, aes(var15)) + geom_freqpoly(binwidth = 1) + facet_grid(~ TARGET)



# ------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------
feature.names <- names(train)
feature.names <- feature.names[-match(c('ID', 'TARGET'), feature.names)]

for (index in 1:length(feature.names)) {
  fn <- feature.names[index]

  title <- ''
  tt <- table(train[[fn]])
  tt <- tt[order(tt, decreasing = T)]
  drop.rows <- c(rep(F, nrow(train)))
  if (tt[1] > 10000) {
    cn <- names(tt[1])
    drop.rows <- train[[fn]] == as.numeric(cn)
    title <- paste0('Dropped ', tt[1], ' values ', cn)
  }

  # drop.rows <- !(train[[fn]] < 23 | train[[fn]] > 35)

  var.factor <- factor(train[!drop.rows, fn])
  var.factor.qty <- length(levels(var.factor))
  if (var.factor.qty > 0) {
    if (var.factor.qty < 500) {
      g <- ggplot(data = train[!drop.rows, ], aes_string(paste0('factor(', fn, ')')))
      # g <- ggplot(data = train, aes_string(paste0('factor(', fn, ')')))
      g <- g + geom_histogram(alpha = .5, col='black')
    } else {
      g <- ggplot(data = train[!drop.rows, ], aes_string(fn))
      g <- g + geom_histogram(binwidth = max(train[[fn]]) / 500, alpha = .5, col='black')
    }

    g <- g +
      facet_grid(TARGET ~ ., scales = 'free_y') +
      ggtitle(title)
    
    ggsave(paste0('plots/', index, '-', fn, '.png'), g, width = 25, height = 10, scale = 1, dpi = 72, limitsize = F)
  }
}


table(train[train$var15 < 23, 'TARGET'])
