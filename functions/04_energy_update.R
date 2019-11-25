energy_update <- function(energy, index, n.act, nodes, n.tot) {
  
  energy[index] <- energy_node(nodes[index], n.act[index], n.tot[index])
  
  above <- index - 1
  below <- index + 1
  left <- index - nrow(energy)
  right <- index + nrow(energy)
  
  if (above > 0 && above <= length(nodes)) {
    energy[above] <- energy_node(nodes[above], n.act[above], n.tot[above])
  }
  
  if (below > 0 && below <= length(nodes)) {
    energy[below] <- energy_node(nodes[below], n.act[below], n.tot[below])
  }
  
  if (left > 0 && left <= length(nodes)) {
    energy[left] <- energy_node(nodes[left], n.act[left], n.tot[left])
  }
  
  if (right > 0 && right <= length(nodes)) {
    energy[right] <- energy_node(nodes[right], n.act[right], n.tot[right])
  }
  
  return(energy)
}