energy_expected <- function(theta, alpha) {
  
  energy.expected <- -4 * (alpha - sqrt(alpha)) * (2 * theta - 1) ^ 2
  
  return(energy.expected)
}