energy_max <- function(theta, alpha) {
  
  ln.energy.max <- (alpha / 2) * (log(theta) + log(1 - theta) + 2 * log(2))
  energy.max <- exp(ln.energy.max)
  
  return(energy.max)
}