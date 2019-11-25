energy_node <- function(node, n.act, n.tot) {
  
  energy <- -4 * node * n.act + 2 * node * n.tot + 2 * n.act - n.tot
  
  return(energy)
}