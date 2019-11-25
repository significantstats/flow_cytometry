split_mat <- function(input, max.res) {
  
  pruned.status <- vector("list", max.res)
  pruned.status[[1]] <- "FALSE"
  
  if (max.res >= 2) {
    for (res in 2:max.res) {
      pruned.status[[res]] <- rep(list("FALSE"), times = 4 ^ (res - 1))
    }
  }
  
  pruned.mat <- vector("list", max.res)
  pruned.mat[[1]] <- list(input)
  
  if (max.res >= 2) {
    for (res in 2:max.res) {
      for (elem in 1:length(pruned.mat[[res - 1]])) {
        no.row <- nrow(pruned.mat[[res - 1]][[1]])
        pruned.mat[[res]][[1 + (elem - 1) * 4]] <- pruned.mat[[res - 1]][[elem]][1:(no.row/2), 1:(no.row/2)]
        pruned.mat[[res]][[2 + (elem - 1) * 4]] <- pruned.mat[[res - 1]][[elem]][1:(no.row/2), (1 + (no.row/2)):no.row]
        pruned.mat[[res]][[3 + (elem - 1) * 4]] <- pruned.mat[[res - 1]][[elem]][(1 + (no.row/2)):no.row, 1:(no.row/2)]
        pruned.mat[[res]][[4 + (elem - 1) * 4]] <- pruned.mat[[res - 1]][[elem]][(1 + (no.row/2)):no.row, (1 + (no.row/2)):no.row]
      }
    }
  }
  
  pruned.alpha <- vector("list", max.res)
  pruned.theta <- vector("list", max.res)
  
  if (max.res >= 2) {
    for (res in 1:max.res) {
      for (elem in 1:length(pruned.mat[[res]])) {
        pruned.alpha[[res]][[elem]] <- length(pruned.mat[[res]][[elem]])
        pruned.theta[[res]][[elem]] <- sum(pruned.mat[[res]][[elem]])
        pruned.theta[[res]][[elem]] <- pruned.theta[[res]][[elem]] / pruned.alpha[[res]][[elem]]
      }
    }
  }
  
  
  output <- structure(list(matrices = pruned.mat, status = pruned.status,
                           alpha = pruned.alpha, theta = pruned.theta))
  
  return(output)
}