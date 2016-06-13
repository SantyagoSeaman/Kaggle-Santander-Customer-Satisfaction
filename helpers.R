save.submission <- function(test.dataset.ids,
                            predicted.values,
                            target.dir,
                            name.prefix,
                            additional.params = c(),
                            timestamp = NULL
                            ) {
  if (is.null(timestamp)) {
    timestamp <- Sys.time()
  }
  submission <- data.frame(ID = test.dataset.ids, TARGET = predicted.values)
  submissionName <- paste(paste(".", target.dir, name.prefix, sep = '/'),
                          format(timestamp, "%H%M%S"),
                          paste(additional.params, collapse = '_'),
                          sep = '_')
  submissionFile <- paste0(submissionName, ".csv")
  write.csv(submission, submissionFile, row.names=FALSE, quote = FALSE)

  return(submissionFile)
}

get.results.mean <- function(results, from = 1, to = NULL) {
  if (is.null(to)) {
    to <- length(results)
  }

  results.mean <- unlist(c(results[from]))

  for(index in (from + 1):to) {
    results.mean <- cbind(results.mean, unlist(c(results[index])))
  }
  results.mean <- rowMeans(results.mean)

  return(results.mean)
}
