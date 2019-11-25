neigh_extend_act_system <- function(nodes) {

  n.act <- matrix(0, nrow = nrow(nodes), ncol = ncol(nodes))
  
  extend <- matrix(0, nrow = nrow(nodes) + 2, ncol = ncol(nodes) + 2)
  extend[2:(nrow(extend) - 1), 2:(ncol(extend) - 1)] <- nodes
  
  for (i in 2:(nrow(extend) - 1)) {
    for (j in 2:(ncol(extend) - 1)) {
      n.act[i - 1, j - 1] <- extend[i - 1, j - 1] + extend[i - 1, j] + 
        extend[i - 1, j + 1] + extend[i, j - 1] + extend[i, j + 1] + 
        extend[i + 1, j - 1] + extend[i + 1, j] + extend[i + 1, j + 1]
    }
  }
  
  return(n.act)
}