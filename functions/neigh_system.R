neigh_system <- function(x, value = 1, dimension = NULL) {
  
  if (is.null(dimension)) {
    dimension <- nrow(x)
  }
  neigh <- matrix(0, nrow = dimension, ncol = dimension)
  for (i in 1:(dimension ^ 2)) {
    neigh[i] <- neighbours(x, i, value, dimension)
  }
    
  return(neigh)
}