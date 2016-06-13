all_results_24_03_2016 <- read.csv("./winner_models/all_results_24_03_2016_igor.csv", header=FALSE, sep=";", stringsAsFactors=FALSE)
all_results_24_03_2016 <- all_results_24_03_2016[!is.na(all_results_24_03_2016$V3), ]
all_results_24_03_2016$V4 <- NULL
names(all_results_24_03_2016) <- c("submit_date", "file_name", "result")
all_results_24_03_2016 <- all_results_24_03_2016[order(all_results_24_03_2016$result, decreasing = F), ]
View(all_results_24_03_2016)



submission.matrix <- data.frame(zeroCol = rep(0, 75818))
for(i in 1:nrow(all_results_24_03_2016)) {
  row <- all_results_24_03_2016[i, ]
  filename <- paste0('./igor/', row$file_name)

  if (file.exists(filename)) {
    submission <- read.csv(filename, stringsAsFactors = F)$TARGET
    submission.matrix <- cbind(submission.matrix, submission)
    colnames(submission.matrix)[ncol(submission.matrix)] <- row$result
  }
}

cols.num <- ncol(submission.matrix)
submission.matrix.sliced <- submission.matrix[, (cols.num-30):cols.num]

submission.deltas <- as.data.frame(t(apply(submission.matrix.sliced, 1, function(row) {
  deltas <- c()
  for(i in 2:length(row)) {
    deltas <- c(deltas, row[i] - row[i-1])
  }
  return(deltas)
})))
View(submission.deltas)

# rle(sign(c(1,-1,-2,2,3,4, 5, 6,7, 0)))

submission.deltas.stats <- as.data.frame(t(apply(submission.deltas, 1, function(row) {
  stat <- table(sign(unlist(row)))
  return(c(stat[1], stat[3]))
})))


View(submission.deltas.stats[submission.deltas.stats$`-1` > 19, ])


results <- apply(submission.deltas.stats, 1, function(row) {
  if (-row['delta_sum'] > 0.01 &
      row['delta_sum'] < 0 &
      row['delta_1'] <= 0 &
      row['delta_2'] <= 0 &
      row['delta_3'] <= 0) {
    return(row['winner_3'] + row['delta_sum']*2 )
  }
  return(row['winner_3'])
})
