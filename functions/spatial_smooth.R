spatial_smooth <- function(nodes, lambda = 0.5) {
  
  no.row <- nrow(nodes)
  no.col <- ncol(nodes)
  
  nodes <- nodes
  
  # Corners
  nodes[1,1] <- nodes[1,1] + lambda * (nodes[1,2] + nodes[2,1]) + lambda * (1/sqrt(2)) * nodes[2,2]
  nodes[1,1] <- nodes[1,1] / (1 + 2 * lambda + lambda / sqrt(2))
  
  nodes[no.row,1] <- nodes[no.row,1] + lambda * (nodes[no.row,2] + nodes[(no.row - 1),1]) + lambda * (1/sqrt(2)) * nodes[(no.row - 1),2]
  nodes[no.row,1] <- nodes[no.row,1] / (1 + 2 * lambda + lambda / sqrt(2))
  
  nodes[1,no.col] <- nodes[1,no.col] + lambda * (nodes[2,no.col] + nodes[1,(no.col - 1)]) + lambda * (1/sqrt(2)) * nodes[2,(no.col - 1)]
  nodes[1,no.col] <- nodes[no.row,1] / (1 + 2 * lambda + lambda / sqrt(2))
  
  nodes[no.row,no.col] <- nodes[no.row,no.col] + lambda * (nodes[no.row,(no.col - 1)] + nodes[(no.row - 1),no.col]) + lambda * (1/sqrt(2)) * nodes[(no.row - 1),(no.col - 1)]
  nodes[no.row,no.col] <- nodes[no.row,no.col] / (1 + 2 * lambda + lambda / sqrt(2))
  
  for (i in 2:(no.row - 1)) {
    nodes[i, 1] <- (nodes[i, 1] + lambda * (nodes[(i - 1),1] + nodes[(i + 1),1] + nodes[i,2]) + lambda * (1/sqrt(2)) * (nodes[(i - 1), 2] + nodes[(i+1),2])) / (1 + 3 * lambda + sqrt(2) * lambda)
    nodes[i,no.col] <- (nodes[i,no.col] + lambda * (nodes[(i-1),no.col] + nodes[(i+1),no.col] + nodes[i,(no.col - 1)]) + lambda * (1/sqrt(2)) * (nodes[(i-1), (no.col - 1)] + nodes[(i + 1),(no.col - 1)])) / (1 + 3 * lambda + sqrt(2) * lambda)  
  }
  
  for (i in 2:(no.col - 1)) {
    nodes[1, i] <- (nodes[1, i] + lambda * (nodes[1, (i - 1)] + nodes[1, (i + 1)] + nodes[2, i]) + lambda * (1/sqrt(2)) * (nodes[2, (i - 1)] + nodes[2, (i + 1)])) / (1 + 3 * lambda + sqrt(2) * lambda)
    nodes[no.row,i] <- (nodes[no.row,i] + lambda * (nodes[no.row,(i-1)] + nodes[no.row,(i+1)] + nodes[(no.row - 1),i]) + lambda * (1/sqrt(2)) * (nodes[(no.row - 1),(i-1)] + nodes[(no.row - 1),(i+1)])) / (1 + 3 * lambda + sqrt(2) * lambda)
  }
  
  for (i in 2:(no.row - 1)) {
    for (j in 2:(no.col - 1)) {
      nodes[i,j] <- nodes[i,j] + lambda * (nodes[(i - 1), j] + nodes[(i + 1), j] + nodes[i, (j - 1)] + nodes[i, (j + 1)]) + lambda * (1/(sqrt(2))) * (nodes[(i - 1), (j - 1)] + nodes[(i - 1), (j + 1)] + nodes[(i + 1), (j + 1)] + nodes[(i + 1), (j - 1)])
      nodes[i,j] <- nodes[i,j]/(1 + 4 * lambda + 2 * sqrt(2) * lambda)
    }
  }  
  
  for (i in (no.row - 1):2) {
    for (j in (no.col - 1):2) {
      nodes[i,j] <- nodes[i,j] + lambda * (nodes[(i - 1), j] + nodes[(i + 1), j] + nodes[i, (j - 1)] + nodes[i, (j + 1)]) + lambda * (1/(sqrt(2))) * (nodes[(i - 1), (j - 1)] + nodes[(i - 1), (j + 1)] + nodes[(i + 1), (j + 1)] + nodes[(i + 1), (j - 1)])
      nodes[i,j] <- nodes[i,j]/(1 + 4 * lambda + 2 * sqrt(2) * lambda)
    }
  }
  
  nodes[nodes < 0.4] <- 0
  nodes[nodes >= 0.4] <- 1
  
  return(nodes)
}