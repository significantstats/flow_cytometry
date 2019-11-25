neigh_extend_tot_system <- function(no.row, no.col) {
  
  n.tot <- matrix(5, nrow = no.row, ncol = no.col)
  
  n.tot[1, 1] <- 3
  n.tot[no.row, 1] <- 3
  n.tot[1, no.col] <- 3
  n.tot[no.row, no.col] <- 3
  
  n.tot[2:(no.row - 1), 2:(no.col - 1)] <- 8
  
  return(n.tot)
}