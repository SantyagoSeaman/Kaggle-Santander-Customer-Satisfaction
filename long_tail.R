#--------------------------------------------------------------
# Внимание! Запускать только на полном датасете!

threshold.features <- c()
feature.names <- names(train)
feature.names <- feature.names[-match(c('ID', 'TARGET'), feature.names)]
for (fname in feature.names) {
  max.positive <- max(train[train$TARGET == 1, fname])
  max.negative <- max(train[train$TARGET == 0, fname])
  positive.qty <- table(train[train[fname] > max.positive, 'TARGET'])[1]
  
  if (!is.na(positive.qty) & positive.qty > 10) {
    cat(fname, ':', positive.qty, ':', max.positive, '\n')
    threshold.features <- c(threshold.features, list(c(fname, positive.qty, max.positive)))
  }
}

threshold.features_df <- data.frame(t(data.frame(threshold.features, stringsAsFactors = F)), stringsAsFactors = F)
names(threshold.features_df) <- c('name', 'qty', 'max')
rownames(threshold.features_df) <- NULL
threshold.features_df$qty <- as.integer(threshold.features_df$qty)
threshold.features_df$max <- as.double(threshold.features_df$max)
threshold.features_df <- threshold.features_df[order(threshold.features_df$qty, decreasing = T), ]



# --------------------------------------------------------------------------------------------
winner_long_tail_122043_num_var43_recib_ult1_60 <- read.csv(
  "./results/winner_long_tail_122043_num_var43_recib_ult1_60",
  stringsAsFactors=FALSE)

# results.mean.final <- partly_predict_13_47_05_2500_0.1_1_0.7_0.8_0.2$TARGET

results.mean.final <- winner_long_tail_122043_num_var43_recib_ult1_60$TARGET

# победители
results.mean.final[test$var38 > 4000000] <- 0
results.mean.final[test$saldo_var30 > 950000] <- 0
results.mean.final[test$var15 < 23] <- 0
results.mean.final[test$saldo_medio_var5_hace3 > 200000] <- 0 # можно попробовать 170000
results.mean.final[test$saldo_medio_var5_hace2 > 170000] <- 0
results.mean.final[test$saldo_medio_var5_ult3 > 120000] <- 0 # можно попробовать 110000
results.mean.final[test$saldo_medio_var5_ult1 > 88000] <- 0
results.mean.final[test$saldo_medio_var13_largo_ult1 > 0] <- 0
results.mean.final[test$num_meses_var13_largo_ult3 > 0] <- 0
results.mean.final[test$saldo_medio_var13_largo_ult3 > 0] <- 0
results.mean.final[test$saldo_medio_var13_largo_hace2 > 0] <- 0
results.mean.final[test$saldo_var5 > 150000] <- 0 # можно попробовать 140000
results.mean.final[test$saldo_medio_var13_largo_hace3 > 0] <- 0
results.mean.final[test$ind_var20_0 > 0] <- 0
results.mean.final[test$saldo_var13_largo > 150000] <- 0
results.mean.final[test$saldo_var20 > 0] <- 0 # ind_var20, num_var20
results.mean.final[test$saldo_medio_var8_hace2 > 15000] <- 0 # проверено 7-20К
results.mean.final[test$imp_aport_var13_ult1 > 150000] <- 0
results.mean.final[test$saldo_medio_var44_hace2 > 0] <- 0 # дубилкаты num_meses_var44_ult3 > 1, num_compra_var44_hace3 > 0
results.mean.final[test$saldo_var14 > 20000] <- 0
results.mean.final[test$num_var13_largo_0 > 3] <- 0
results.mean.final[test$num_aport_var13_hace3 > 3] <- 0
results.mean.final[test$var3 > 200] <- 0
results.mean.final[test$saldo_medio_var13_corto_hace3 > 160000] <- 0
results.mean.final[test$var21 > 7200] <- 0
results.mean.final[test$saldo_var26 > 10500] <- 0 # saldo_var25
results.mean.final[test$ind_var33_0 > 0] <- 0
results.mean.final[test$saldo_var42 > 900000] <- 0 # +0.000001   saldo_var12, saldo_var24
# results.mean.final[test$delta_imp_venta_var44_1y3 == 9999999999] <- 0 #  прирост 8
results.mean.final[test$imp_trans_var37_ult1 > 500000] <- 0 # +0.000026
# Протрезветь и понять, с какого перепуга я это сюда запихнул?!
# results.mean.final[test$num_var14_0 > 5] <- 0
results.mean.final[test$num_var12 > 5] <- 0
results.mean.final[test$imp_ent_var16_ult1 > 57000] <- 0
results.mean.final[test$num_op_var40_comer_ult3 > 50] <- 0 # +0.000073 +56
results.mean.final[test$imp_op_var41_ult1 > 17000] <- 0
results.mean.final[test$imp_op_var39_ult1 > 17000] <- 0
results.mean.final[test$num_venta_var44_ult1 > 0] <- 0 # +44, относительно +4
results.mean.final[test$num_aport_var17_hace3 > 0] <- 0 # относительно +17
results.mean.final[test$saldo_medio_var8_hace3 > 1500] <- 0 # +0.000084 +56, относительно +47
results.mean.final[test$num_op_var39_ult3 > 220] <- 0 # +0.000111, +36, относительно +31
results.mean.final[test$num_op_var41_hace3 > 30] <- 0 # +0.000025, +19, относительно +18
results.mean.final[test$saldo_medio_var12_hace2 > 700000] <- 0 # +0.000008, +28, относительно +15
results.mean.final[test$num_compra_var44_ult1 > 3] <- 0 # относительно +19
results.mean.final[test$num_op_var39_comer_ult1 > 130] <- 0 # +30, относительно +9
results.mean.final[test$num_op_var39_comer_ult3 > 250] <- 0 # +18, относительно +1
results.mean.final[test$saldo_medio_var13_corto_ult3 > 350000] <- 0 # +50, относительно +19
results.mean.final[test$imp_var7_recib_ult1 > 85000] <- 0 # +0.000076 относ +18
results.mean.final[test$num_var43_recib_ult1 > 60] <- 0 #  # +0.000013 > 60, +52
results.mean.final[test$num_var13_corto_0 > 3] <- 0 # относ +22
results.mean.final[test$num_trasp_var11_ult1 > 30] <- 0 # plus > 30, minus  +48
results.mean.final[test$saldo_var30 > 12000 & test$var15 < 28] <- 0



results.mean.final[test$saldo_var30 > 400000 & test$var38 > 100000 & test$var15 > 50 & test$var15 < 60] <- 0


# results.mean.final[test$saldo_var30 > 400000 & test$var15 < 50] <- 0
# results.mean.final[test$saldo_var30 > 100000 & test$var15 > 65] <- 0
# results.mean.final[test$saldo_var30 > 200000 & test$var15 > 70] <- 0
# results.mean.final[test$saldo_var30 > 200000 & test$var15 > 70] <- results.mean.final[test$saldo_var30 > 200000 & test$var15 > 70]/10
# results.mean.final[test$saldo_var30 > 400000 & test$var15 > 50] <- 0


# zzz <- which(test$var15 < 23)
# zzz2 <- which(test$saldo_var42 > 800000)
# table(train[train$num_var12 > 10, 'TARGET'])
# setdiff(zzz, zzz2)
# length(results.mean.final[results.mean.final == 0])
# table(train[train$num_var12 > 5, 'TARGET'])
# table(train[train$num_var14_0 > 5, 'TARGET'])

# --------------------------------------------------------------------------
results.mean.final <- xggb_163250_4_4$TARGET


results.mean.final[test$num_meses_var17_ult3 > 1] <- 0 # -0.0002 +87  покрывает num_meses_var33_ult3
# results.mean.final[test$num_meses_var33_ult3 > 0] <- 0 # +48


# results.mean.final[test$saldo_var13 > 420000] <- 0 # minus at 400000, minus as 420000
# results.mean.final[test$imp_aport_var13_hace3 > 420000] <- 0 # minus at 400000, dublicate saldo_var13
# results.mean.final[test$saldo_medio_var13_corto_hace2 > 420000] <- 0 # minus at 400000, 320000 -0.0003, +107, dublicate saldo_var13
# results.mean.final[test$saldo_medio_var13_corto_ult1 > 420000] <- 0 # minus at 400000, относ +90 на >320000 ,-0.000246 на 309000, dublicate saldo_var13


# results.mean.final[test$saldo_var33 > 0] <- 0 # saldo_medio_var33_ult1, saldo_medio_var33_ult3
results.mean.final[test$saldo_var1 > 5000] <- 0 # +15, minus >4600

# results.mean.final[test$num_var12 > 5] <- 0
# results.mean.final[test$num_var13 > 6] <- 0
# results.mean.final[test$num_var14_0 > 5 & test$num_var14_0 < 100] <- 0

# ни в коем случае не трогать фичи даже с подозрением на наличие позитивных результатов
# results.mean.final[test$num_var17_0 > 0] <- results.mean.final[test$num_var17_0 > 0] / 10 # глубоко в жопе

# найти imp_op_var39_comer_ult3
# найти imp_op_var41_comer_ult3

# results.mean.final[test$num_op_var40_comer_ult1 > 40] <- 0 # -0.000587 >40, minus at 35 +30, относительно +11

# покрываются предыдущими
# results.mean.final[test$num_op_var41_comer_ult1 > 130] <- 0 # +23
# results.mean.final[test$num_op_var41_comer_ult3 > 250] <- 0 # +15

# results.mean.final[test$saldo_medio_var8_ult1 > 700000] <- 0 # +20, относительно +0

# results.mean.final[test$saldo_medio_var12_hace3 > 200000] <- 0 # minus, 120000 -0.0003 +95, проверить на > 150000
# results.mean.final[test$saldo_medio_var12_ult1 > 800000] <- 0 # minus, +45, относительно +0, -0.000171 +41 на >600000
# results.mean.final[test$saldo_medio_var12_ult3 > 550000] <- 0 # minus, +52, относительно +12

# покрываются предыдущими
# results.mean.final[test$saldo_medio_var17_hace3 > 0] <- 0 # +17
# results.mean.final[test$saldo_medio_var33_hace2 > 0] <- 0 # +43
# results.mean.final[test$saldo_medio_var33_hace3 > 0] <- 0 # +24
# results.mean.final[test$saldo_medio_var44_hace3 > 0] <- 0 # +32

# results.mean.final[test$var3 > 170] <- 0
# results.mean.final[test$var3 > 60] <- 0
table(train[train$var3 > 190, 'TARGET'])
length(results.mean.final[results.mean.final == 0])



results.mean.final[test$num_var30 > 9] <- 0 # +38, относительно +2
# results.mean.final[test$num_var13_0 > 6] <- 0 # +32, относительно +0
# results.mean.final[test$num_var33_0 > 0] <- 0 # +57, относительно +0, вероятно дубликат ind_var33_0

results.mean.final[test$saldo_var32 > 4600] <- 0 # -0.001 +17, относительно +18
# results.mean.final[test$num_op_var39_hace3 > 30] <- 0 # +20, относительно +0
# results.mean.final[test$imp_op_var39_efect_ult3 > 15000] <- 0 # minus, +17, относительно +9

# results.mean.final[test$num_op_var41_ult3 > 220] <- 0 # +31, относительно +0
# results.mean.final[test$imp_venta_var44_ult1 > 0] <- 0 # +44, относительно +0

results.mean.final[test$saldo_medio_var44_ult1 > 37000] <- 0 # относ +2
# results.mean.final[test$delta_imp_venta_var44_1y3 > 0] <- 0 # относ +0
# results.mean.final[test$delta_num_venta_var44_1y3 > 0] <- 0 # относ +0
# results.mean.final[test$imp_compra_var44_hace3 > 0] <- 0 # относ +0
# results.mean.final[test$num_op_var40_hace2 > 20] <- 0 # minus, minus >12, относ 19
results.mean.final[test$imp_op_var40_comer_ult3 > 3700] <- 0 # относ 9


# отправленные относительно последнего победителя топ-7
results.mean.final[test$var3 > 142] <- 0 # -0.000206
# results.mean.final[test$saldo_medio_var13_corto_ult3 > 350000] <- 0 # -0.000102 что за фигня? почему здесь минус?
# results.mean.final[test$saldo_medio_var17_hace2 > 10000] <- 0 # minus на >10000, -0.000268 +89 на > 0

# слабые
# results.mean.final[test$num_var4 > 6] <- 0
# results.mean.final[test$num_var35 > 23] <- 0 # попробовать позже
# results.mean.final[test$saldo_var42 > 600000] <- 0


# перепроверить
imp_aport_var13_ult1
saldo_var5
saldo_medio_var5_ult3
ind_var33
num_var33
num_var4
num_op_var40_comer_ult1


table(train[train$num_var35 > 23, 'TARGET'])
length(results.mean.final[results.mean.final == 0])


results.mean.final <- xggb_163250_4_4$TARGET


# -------------------------------------------------------------------------------
save.submission(test$ID, results.mean.final, 'results', 'winner_long_tail',
                c('current_all'))

save.submission(test$ID, results.mean.final, 'results', 'winner_long_tail',
                c('saldo_var30', 400000, 'var38', 100000, 'var15', 50, 60))


# -------------------------------------------------------------------------------
partly_predict_13_47_05_2500_0.1_1_0.7_0.8_0.2 <- read.csv(
  "./results/partly_predict_13_47_05_2500_0.1_1_0.7_0.8_0.2.csv",
  stringsAsFactors=FALSE)
results.mean.final <- partly_predict_13_47_05_2500_0.1_1_0.7_0.8_0.2$TARGET
results.mean.final[test.zero$is_zero == 1] <- results.mean.final[test.zero$is_zero == 1] / 10

save.submission(test$ID, results.mean.final, 'results', 'winner_long_tail',
                c('current_all', 'devided', 'by', 10))


# --------------------------------------------------------------------------------------------
# results.mean.final <- partly_predict_13_47_05_2500_0.1_1_0.7_0.8_0.2$TARGET
# 
# for(name in threshold.features_df[threshold.features_df$qty > 100, 'name']) {
#   th <- round(1.1 * threshold.features_df[threshold.features_df$name == name, 'max'], 0)
#   results.mean.final[test[[name]] > th] <- 0
# }
# 
# length(results.mean.final[results.mean.final == 0])
# 
# save.submission(test$ID, results.mean.final, 'results', 'winner_long_tail', c('total_100'))


# --------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------
0.840969

lt_233727_num_var35_21_0.00907.csv	                        0.840341	
lt_233710_num_var4_5_0.00914.csv	                          0.840376
lt_233651_saldo_medio_var5_hace2_170000_0.00905.csv         0.841007	
lt_233633_saldo_medio_var5_hace3_20000_0.00908.csv          0.839963	
lt_233610_saldo_var30_510000_0.00905.csv                    0.840555  -

lt_152105_saldo_medio_var13_largo_ult1_0_0.1_0.00908.csv    0.841161
lt_135407_saldo_var5_150000_0.00908.csv                     0.841036
lt_135251_saldo_medio_var5_ult1_90000_0.00908.csv           0.841129
lt_135140_saldo_medio_var5_ult3_120000_0.00908.csv          0.841028
lt_133258_saldo_var30_550000_0.00907.csv                    0.840543

lt_131743_imp_aport_var13_hace3_250000.csv                  0.840626  -
lt_131726_saldo_medio_var13_largo_hace2_0.csv               0.841111
lt_131715_saldo_medio_var13_largo_ult3_0.csv                0.841162
lt_131449_num_meses_var13_largo_ult3_0.csv                  0.841164
lt_131220_saldo_medio_var13_largo_ult1_0.csv                0.841162

winner_long_tail_234652_saldo_medio_var13_largo_hace3_0.csv	0.841029
winner_long_tail_234652_ind_var20_0_0.csv	                  0.841071  num_var20_0


lt_185149_saldo_medio_var13_corto_hace2_287065.csv          0.840617
lt_184948_saldo_var13_corto_309000.csv                      0.840631
lt_184503_saldo_var20_0.csv                                 0.841005
lt_184304_saldo_var13_309000.csv                            0.840646
lt_184150_saldo_var13_largo_150000.csv                      0.841021


lt_144550_var3_150.csv                                      0.840789
lt_144433_imp_aport_var13_ult1_150000.csv                   0.841037
lt_144319_saldo_medio_var13_corto_ult1_400000.csv	          0.840832
lt_144123_imp_aport_var13_hace3_400000.csv                  0.840814 можно попробовать >500000
lt_143951_saldo_var30_800000.csv                            0.840733

lt_230159_num_var13_largo_0_3.csv                           0.840998  перепроверить
lt_230050_saldo_medio_var12_hace3_100000.csv                0.840777
lt_225939_num_var43_recib_ult1_30.csv                       0.839897
lt_225801_num_aport_var13_hace3_3.csv                       0.840992  перепроверить
lt_225339_var3_200.csv                                      0.840995  перепроверить

lt_164720_saldo_medio_var13_corto_hace3_160000.csv          0.840981
lt_164132_saldo_var1_4600.csv                               0.839641
lt_164049_var21_7200.csv                                    0.840994
lt_163312_saldo_var26_10500.csv                             0.841057
lt_162755_ind_var33_0_0.csv                                 0.841122

lt_103103_num_var43_recib_ult1_40.csv                       0.840475
lt_102744_num_var17_0_0_0.1.csv                             0.840308
lt_102436_num_var14_0_5_100.csv                             0.840982  ????????? как оно сюда попало?
lt_101914_num_var12_5.csv                                   0.840994
lt_100202_imp_ent_var16_ult1_57000.csv                      0.840978

lt_111044_num_trasp_var11_ult1_20.csv                       0.840649
lt_110308_num_venta_var44_ult1_0.csv                        0.841031
lt_110150_num_var43_recib_ult1_50.csv                       0.840419
lt_105932_num_aport_var17_hace3_0.csv                       0.841006
lt_100822_imp_op_var41_ult1_imp_op_var39_ult1_17000.csv     0.841047

lt_080338_num_op_var40_comer_ult1_35.csv                    0.840455
lt_080254_num_compra_var44_ult1_3.csv                       0.841082
lt_080129_num_op_var39_comer_ult1_130_num_op_var39_comer_ult3_250.csv  0.841158
lt_075221_saldo_medio_var13_corto_hace2_400000.csv          0.840813
lt_074944_saldo_medio_var13_corto_ult3_350000.csv           0.840991

lt_115909_saldo_medio_var13_corto_ult3_350000.csv           0.840991
lt_114502_imp_op_var40_comer_ult3_3700.csv                  0.840418
lt_114427_num_op_var40_hace2_12.csv                         0.840433
lt_114340_imp_var7_recib_ult1_85000.csv                     0.840969
lt_114309_num_var13_corto_0_3.csv                           0.840984


lt_131804_num_op_var40_hace2_20.csv                         0.840379
lt_131648_imp_op_var39_efect_ult3_15000.csv                 0.840413
lt_131555_saldo_medio_var12_hace3_200000.csv                0.840745
lt_131420_saldo_medio_var12_ult3_550000.csv                 0.840739
lt_131321_saldo_medio_var12_ult1_800000.csv                 0.840733

lt_131937_saldo_var1_5000.csv                               0.839628
lt_132128_num_trasp_var11_ult1_30.csv                       0.841012
lt_133003_saldo_var30_900000.csv                            0.840977
lt_133145_saldo_medio_var5_ult1_85000.csv                   0.841137
lt_133410_var3_170.csv                                      0.840689


