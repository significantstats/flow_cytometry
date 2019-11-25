neigh_act_update <- function(n.act, nodes, index) {
  
  above <- index - 1
  below <- index + 1
  left <- index - nrow(nodes)
  right <- index + nrow(nodes)
  
  if (above > 0 && above <= length(nodes)) {
    n.act[above] <- neigh_act_node(nodes, above)
  }
  
  if (below > 0 && below <= length(nodes)) {
    n.act[below] <- neigh_act_node(nodes, below)
  }
  
  if (left > 0 && left <= length(nodes)) {
    n.act[left] <- neigh_act_node(nodes, left)
  }
  
  if (right > 0 && right <= length(nodes)) {
    n.act[right] <- neigh_act_node(nodes, right)
  }
  
  return(n.act)
}