neigh_tot_system <- function(no.row, no.col) {
  
  n.tot <- matrix(3, nrow = no.row, ncol = no.col)
  
  n.tot[1, 1] <- 2
  n.tot[no.row, 1] <- 2
  n.tot[1, no.col] <- 2
  n.tot[no.row, no.col] <- 2
  
  n.tot[2:(no.row - 1), 2:(no.col - 1)] <- 4
  
  return(n.tot)
}