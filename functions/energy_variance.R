energy_variance <- function(theta, alpha) {
  
  alpha.sqrt <- sqrt(alpha)
  energy.var <- (14 * alpha - 26 * alpha.sqrt + 8) * (2 * theta - 1) ^ 2
  energy.var <- energy.var + 2 * alpha - 2 * alpha.sqrt
  energy.var <- 16 * theta * (1 - theta) * energy.var
  
  return(energy.var)
}