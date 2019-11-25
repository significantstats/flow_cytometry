energy_var <- function(nodes, alpha, theta) {
  
  neigh.act <- neigh_act_system(nodes = nodes)
  neigh.tot <- neigh_tot_system(no.row = nrow(nodes), no.col = ncol(nodes))
  
  energy.obs <- energy_system(nodes = nodes, n.act = neigh.act,
                              n.tot = neigh.tot)
  
  energy.expected <- energy_expected(theta = theta, alpha = alpha)
  energy.variance <- energy_variance(theta = theta, alpha = alpha)
  
  energy.var <- (sum(energy.obs) - energy.expected) / sqrt(energy.variance)
  
  return(energy.var)
}