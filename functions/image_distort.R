image_distort <- function(state, proportion) {
  
  distort.mat <- matrix(sample(c(0, 1), size = length(state), replace = TRUE, 
                               prob = c(1 - proportion, proportion)), 
                        nrow = nrow(state), ncol = ncol(state))
  
  state.noise <- state - distort.mat * (2 * state - 1)
    
  return(state.noise)  
}