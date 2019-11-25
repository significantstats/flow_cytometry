prob_update <- function(prob, energy, theta, n.tot, index) {
  
  col.ind <- ceiling(index / nrow(prob))
  row.ind <- index - (col.ind - 1) * nrow(prob)
  
  col.inds <- c(col.ind - 1, col.ind, col.ind + 1)
  row.inds <- c(row.ind - 1, row.ind, row.ind + 1)
  
  col.inds <- col.inds[which(col.inds >= 1)]
  row.inds <- row.inds[which(row.inds >= 1)]
  
  col.inds <- col.inds[which(col.inds <= ncol(prob))]
  row.inds <- row.inds[which(row.inds <= nrow(prob))]
  
  indices <- rep(row.inds, times = length(col.inds)) +
    sort(rep(col.inds, times = length(row.inds)) - 1) * nrow(prob)
  
  indices <- indices[which(indices != index)]
  
  for (i in 1:length(indices)) {
    prob[indices[i]] <- prob_node(energy[indices[i]], theta[indices[i]], n.tot[indices[i]])
  }
  
  return(prob)
}