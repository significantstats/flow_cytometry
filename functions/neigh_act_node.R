neigh_act_node <- function(nodes, index) {
  
  n.act <- 0
  
  above <- index - 1
  below <- index + 1
  left <- index - nrow(nodes)
  right <- index + nrow(nodes)
  
  if (above > 0 && above %% nrow(nodes) != 0) {
    if (nodes[above] == 1) {
      n.act <- n.act + 1
    }
  }
  
  if (below <= length(nodes) && below %% nrow(nodes) != 1) {
    if (nodes[below] == 1) {
      n.act <- n.act + 1
    }
  }
  
  if (left >= 1) {
    if (nodes[left] == 1) {
      n.act <- n.act + 1
    }
  }
  
  if (right <= length(nodes)) {
    if (nodes[right] == 1) {
      n.act <- n.act + 1
    }
  }
  
  return(n.act)
}