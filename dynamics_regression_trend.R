library(itsmr)


all_results <- read.csv("./winner_models/all_results_03_04_2016.csv", header=FALSE, sep=";", stringsAsFactors=FALSE)
all_results <- all_results[!is.na(all_results$V3), ]
all_results$V4 <- NULL
names(all_results) <- c("submit_date", "file_name", "result")
all_results <- all_results[order(all_results$result, decreasing = F), ]
View(all_results)



submission.matrix <- data.frame(zeroCol = rep(0, 75818))
for(i in 1:nrow(all_results)) {
  row <- all_results[i, ]
  filename <- paste0('./results/', row$file_name)
  
  if (file.exists(filename)) {
    submission <- read.csv(filename, stringsAsFactors = F)$TARGET
    submission.matrix <- cbind(submission.matrix, submission)
    colnames(submission.matrix)[ncol(submission.matrix)] <- row$result
  }
}

cols.num <- ncol(submission.matrix)
submission.matrix.sliced <- submission.matrix[, (cols.num-120):cols.num]

submission.deltas <- as.data.frame(t(apply(submission.matrix.sliced, 1, function(row) {
  deltas <- c()
  for(i in 2:length(row)) {
    deltas <- c(deltas, row[i] - row[i-1])
  }
  return(deltas)
})))

View(submission.deltas)

# ---------------------------------------------------------------------------------
deltas.column.sums <- colSums(submission.deltas)
names(deltas.column.sums) <- names(submission.deltas)
head(deltas.column.sums, 20)

deltas.column.remove.indexes <- which(abs(deltas.column.sums) - 0.00001 < 0 | deltas.column.sums < -1000 | deltas.column.sums > 1000)

head(deltas.column.sums[deltas.column.remove.indexes], 20)


submission.matrix.sliced <- submission.matrix.sliced[, -(deltas.column.remove.indexes+1)]
submission.deltas <- submission.deltas[, -deltas.column.remove.indexes]


submission.matrix.sliced.cols <- ncol(submission.matrix.sliced)
submission.matrix.sliced.names <- names(submission.matrix.sliced)

# ---------------------------------------------------------------------------------


# rle(sign(c(1,-1,-2,2,3,4, 5, 6,7, 0)))

# ---------------------------------------------------------------------------------
submission.deltas.stats <- as.data.frame(t(apply(submission.deltas, 1, function(row) {
  stat <- table(sign(unlist(row)))
  return(c(stat[1], stat[3]))
})))

submission.deltas.stats$diff <- submission.deltas.stats$`1` - submission.deltas.stats$`-1`

submission.deltas.stats$deltaSums <- rowSums(submission.deltas)
submission.deltas.stats$deltaMeans <- rowMeans(submission.deltas)

submission.deltas.stats$trend_coef_20 <- apply(submission.matrix.sliced[, tail(submission.matrix.sliced.names, 20)], 1, function(row) {
  t <- trend(unlist(row), 1)
  return(t[length(row)]/t[1])
})
submission.deltas.stats$trend_coef_50 <- apply(submission.matrix.sliced[, tail(submission.matrix.sliced.names, 50)], 1, function(row) {
  t <- trend(unlist(row), 1)
  return(t[length(row)]/t[1])
})
submission.deltas.stats$trend_coef_100 <- apply(submission.matrix.sliced[, tail(submission.matrix.sliced.names, 100)], 1, function(row) {
  t <- trend(unlist(row), 1)
  return(t[length(row)]/t[1])
})


deltas.row <- as.data.frame(t(submission.deltas[111:113, ]))
deltas.row <- as.data.frame(t(submission.deltas[head(which(submission.deltas.stats$trend_coef_20 > 1.2), 3), ]))
deltas.row <- as.data.frame(t(submission.deltas[head(which(submission.deltas.stats$deltaMeans < 0.001), 3), ]))
deltas.row <- as.data.frame(t(submission.deltas[head(which(submission.deltas.stats$diff > 20), 3), ]))
deltas.row <- tail(deltas.row, 500)
names(deltas.row) <- c('delta1', 'delta2', 'delta3')
deltas.row$labels <- rownames(deltas.row)
g <- ggplot(deltas.row) +
  # scale_x_discrete() + #scale_y_discrete(breaks = seq(0, 50000, 1000)) +
  # geom_smooth(data = zzz, size = 1, method="loess", alpha = 0.2) +
  geom_smooth(aes(x = labels, y = delta1, group = 1), method="loess", alpha = 0.2) +
  geom_smooth(aes(x = labels, y = delta1, group = 1), method="lm", alpha = 0.2, color = 'red', se = FALSE, fullrange=TRUE) +
  geom_smooth(aes(x = labels, y = delta2, group = 1), method="lm", alpha = 0.2, color = 'green', se = FALSE, fullrange=TRUE) +
  geom_smooth(aes(x = labels, y = delta3, group = 1), method="lm", alpha = 0.2, color = 'yellow', se = FALSE, fullrange=TRUE) +
  geom_point(aes(x = labels, y = delta1, alpha = 0.9), color = 'red', size = 4) +
  geom_point(aes(x = labels, y = delta2, alpha = 0.9), color = 'green', size = 4) +
  geom_point(aes(x = labels, y = delta3, alpha = 0.9), color = 'yellow', size = 4)
g




View(submission.deltas.stats[submission.deltas.stats$diff > 10 & submission.deltas.stats$deltaSums > 0.1, ])
View(submission.matrix.sliced[which(submission.deltas.stats$diff > 20), submission.matrix.sliced.names[50:112]])
View(submission.matrix.sliced[which(submission.deltas.stats$trend_coef_20 > 1.2), submission.matrix.sliced.names[90:112]])
View(submission.matrix.sliced[which(submission.deltas.stats$trend_coef_20 < 0.3), submission.matrix.sliced.names[20:112]])
View(submission.matrix.sliced[which(submission.deltas.stats$trend_coef_20 > 1.5 & submission.deltas.stats$deltaSums > 0.1), submission.matrix.sliced.names[90:112]])
View(submission.matrix.sliced[which(submission.deltas.stats$trend_coef_20 < 0.5 & submission.deltas.stats$deltaSums < -0.05), submission.matrix.sliced.names[90:112]])




submission <- data.frame(ID = test$ID, TARGET = submission.matrix.sliced[, 112])
indexes <- which(submission.deltas.stats$trend_coef_20 < 0.3 & submission.matrix.sliced$`0.840225` < 0.1 )
submission[indexes, 'TARGET'] <- submission[indexes, 'TARGET']/2


submissionName <- paste("./results/regression_minus_trend_experiment", format(Sys.time(), "%H_%M_%S"), 112, 0.3, 2, sep= '_')
submissionFile <- paste0(submissionName, ".csv")
write.csv(submission, submissionFile, row.names=FALSE, quote = FALSE)




