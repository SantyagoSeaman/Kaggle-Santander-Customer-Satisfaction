all_results_24_03_2016 <- read.csv("./winner_models/all_results_24_03_2016.csv", header=FALSE, sep=";", stringsAsFactors=FALSE)
all_results_24_03_2016 <- all_results_24_03_2016[!is.na(all_results_24_03_2016$V3), ]
all_results_24_03_2016$V4 <- NULL
names(all_results_24_03_2016) <- c("submit_date", "file_name", "result")
all_results_24_03_2016 <- all_results_24_03_2016[order(all_results_24_03_2016$result, decreasing = F), ]
View(all_results_24_03_2016)



submission.matrix <- data.frame(zeroCol = rep(0, 75818))
for(i in 1:nrow(all_results_24_03_2016)) {
  row <- all_results_24_03_2016[i, ]
  filename <- paste0('./results/', row$file_name)
  
  if (file.exists(filename)) {
    submission <- read.csv(filename, stringsAsFactors = F)$TARGET
    submission.matrix <- cbind(submission.matrix, submission)
    colnames(submission.matrix)[ncol(submission.matrix)] <- row$result
  }
}

cols.num <- ncol(submission.matrix)
cols.names <- colnames(submission.matrix)
submission.matrix.sliced <- submission.matrix[, (cols.num-50):cols.num]

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

submission.deltas.stats$deltaSums <- rowSums(submission.deltas)


# ========================================================================================================================

View(submission.deltas.stats[submission.deltas.stats$`-1` > 12 & submission.deltas.stats$deltaSums < 0, ])
View(submission.deltas[submission.deltas.stats$`-1` > 28, ])
View(submission.deltas[submission.deltas.stats$`-1` > 13, ])
View(submission.matrix.sliced[submission.deltas.stats$`-1` > 28, ])
View(submission.matrix.sliced[submission.deltas.stats$`-1` > 25 & submission.deltas.stats$deltaSums < 0 & submission.matrix.sliced[, 50] < 0.013, ])


submission <- data.frame(ID = test$ID, TARGET = submission.matrix.sliced[, 50])
zzz <- submission[submission.deltas.stats$`-1` > 25 & submission.deltas.stats$deltaSums < 0 & submission.matrix.sliced[, 50] < 0.013, 'TARGET']
submission[submission.deltas.stats$`-1` > 25 & submission.deltas.stats$deltaSums < 0 & submission.matrix.sliced[, 50] < 0.013, 'TARGET'] <- zzz/10


submissionName <- paste("./results/regression_minus", format(Sys.time(), "%H_%M_%S"), 50, 25, 0, 0.013, 10, sep= '_')
submissionFile <- paste0(submissionName, ".csv")
write.csv(submission, submissionFile, row.names=FALSE, quote = FALSE)
