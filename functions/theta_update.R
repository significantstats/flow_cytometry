theta_update <- function(theta, nodes, n.act, n.tot, index) {
  
  col.ind <- ceiling(index / nrow(nodes))
  row.ind <- index - (col.ind - 1) * nrow(nodes)
  
  col.inds <- c(col.ind - 1, col.ind, col.ind + 1)
  row.inds <- c(row.ind - 1, row.ind, row.ind + 1)
  
  col.inds <- col.inds[which(col.inds >= 1)]
  row.inds <- row.inds[which(row.inds >= 1)]
  
  col.inds <- col.inds[which(col.inds <= ncol(nodes))]
  row.inds <- row.inds[which(row.inds <= nrow(nodes))]
  
  indices <- rep(row.inds, times = length(col.inds)) +
    sort(rep(col.inds, times = length(row.inds)) - 1) * nrow(nodes)
  
  indices <- indices[which(indices != index)]
  
  for (i in 1:length(indices)) {
    theta[indices[i]] <- (n.act[indices[i]] + nodes[indices[i]]) / (n.tot[indices[i]] + 1)
  }
  
  return(theta)
}