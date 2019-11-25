prob_node <- function(energy, theta, n.tot) {
  
  if (n.tot == 4) {
    if (energy == -4) {
      prob <- theta ^ 5 + (1 - theta) ^ 5
    } else if (energy == -2) {
      prob <- 4 * theta * (1 - theta) * ((1 - theta) ^ 3 + theta ^ 3)
    } else if (energy == 0) {
      prob <- 6 * theta ^ 2 * (1 - theta) ^ 2
    } else if (energy == +2) {
      prob <- 4 * theta ^ 2 * (1 - theta) ^ 2
    } else if (energy == +4) {
      prob <- theta * (1 - theta) * ((1 - theta) ^ 3 + theta ^ 3)
    } else{
      stop("energy must be one of -4, -2, 0, +2 or +4, when n.tot is 4.")
    }
  } else if (n.tot == 3) {
    if (energy == -3) {
      prob <- theta ^ 4 + (1 - theta) ^ 4
    } else if (energy == -1) {
      prob <- 3 * theta * (1 - theta) * ((1 - theta) ^ 2 + theta ^ 2)
    } else if (energy == +1) {
      prob <- 6 * theta ^ 2 * (1 - theta) ^ 2
    } else if (energy == +3) {
      prob <- theta * (1 - theta) * ((1 - theta) ^ 2 + theta ^ 2)
    } else{
      stop("energy must be one of -3, -1, +1 or +3, when n.tot is 3.")
    }
  } else if (n.tot == 2) {
    if (energy == -2) {
      prob <- theta ^ 3 + (1 - theta) ^ 3
    } else if (energy == 0) {
      prob <- 2 * theta * (1 - theta)
    } else if (energy == +2) {
      prob <- theta * (1 - theta)
    } else{
      stop("energy must be one of -2, 0 or +2, when n.tot is 2.")
    }
  } else {
    stop("n.tot must be one of 2, 3 or 4.")
  }
  
  return(prob)
}