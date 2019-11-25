theta_node <- function(node, n.act, n.tot) {
  
  theta <- (n.act + node) / (n.tot + 1)
  
  return(theta)
}