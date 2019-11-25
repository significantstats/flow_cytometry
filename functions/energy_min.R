energy_min <- function(theta, alpha) {
  
  energy.min <- theta ^ alpha + (1 - theta) ^ alpha
  
  return(energy.min)
}