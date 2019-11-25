theta_system <- function(nodes, n.act, n.tot) {
  
  theta <- (n.act + nodes) / (n.tot + 1)
  
  return(theta)
}