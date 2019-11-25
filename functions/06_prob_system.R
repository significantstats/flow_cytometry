prob_system <- function(energy, theta, n.tot) {
  
  prob <- matrix(0, nrow = nrow(energy), ncol = ncol(energy))
  
  for (i in 1:length(prob)) {
    prob[i] <- prob_node(energy[i], theta[i], n.tot[i])
  }
  
  return(prob)
}