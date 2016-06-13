all.winners <- winner_11_26
all.winners$winner_1 <- xg_grid_cv_bag_09_38_0247$TARGET
all.winners$winner_2 <- h2o_deeplearning_bag_winner841287_01_44_15$TARGET
all.winners$winner_3 <- top_ensemble_deep_winner841287_02_08$TARGET

all.winners$delta_1 <- all.winners$winner_1 - all.winners$TARGET
all.winners$delta_2 <- all.winners$winner_2 - all.winners$winner_1
all.winners$delta_3 <- all.winners$winner_3 - all.winners$winner_2

all.winners$delta_sum <- rowSums(all.winners[, c('delta_1', 'delta_2', 'delta_3')])

View(all.winners[all.winners$delta_sum > 0.005 &
                   all.winners$delta_1 > 0 &
                   all.winners$delta_2 > 0 &
                   all.winners$delta_3 > 0
                   , ])

View(all.winners[all.winners$delta_sum < 0 &
                   -all.winners$delta_sum > 0.01 &
                   all.winners$delta_1 <= 0 &
                   all.winners$delta_2 <= 0 &
                   all.winners$delta_3 <= 0
                 , ])


all.winners$final <- apply(all.winners, 1, function(row) {
  if (-row['delta_sum'] > 0.01 &
      row['delta_sum'] < 0 &
      row['delta_1'] <= 0 &
      row['delta_2'] <= 0 &
      row['delta_3'] <= 0) {
    return(row['winner_3'] + row['delta_sum']*2 )
  }
  return(row['winner_3'])
})

submission <- data.frame(ID = test$ID, TARGET = all.winners$final)
submissionName <- paste("./results/regression_minus",
                        format(Sys.time(), "%H_%M_%S"),
                        0.01, 2,
                        sep = '_')
submissionFile <- paste0(submissionName, ".csv")
write.csv(submission, submissionFile, row.names=FALSE, quote = FALSE)


# model_lm <- lm(winner_3 ~ TARGET + winner_1 + winner_2, all.winners[1, ])
# summary(model_lm)
