energy_system <- function(nodes, n.act, n.tot) {
  
  energy <- -4 * nodes * n.act + 2 * nodes * n.tot + 2 * n.act - n.tot
  
  return(energy)
}