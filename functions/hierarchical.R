hierarchical <- function(state, temp = 4) {
  
  # Lattice Dimension
  no.row <- nrow(state)
  no.col <- ncol(state)
  
  # Resolution Levels (stopping at 8 x 8 matrices)
  max.res <- (log2(no.row) + 1) - 3
  
  # Holding List for Pruning Status, Matrices, theta & alpha
  pruned <- split_mat(input = state, max.res = max.res)
  
  
  # Loop to iterate through each branch of the hierarchical structure
  for (res in 2:max.res) {
    for (elem in 1:length(pruned$status[[res]])) {
      
      # If pruned status = FALSE then apply the algorithm
      if (pruned$status[[res]][[elem]] == "FALSE") {
      
        # Required lattice properties
        alpha.prev <- pruned$alpha[[res - 1]][[ceiling(elem / 4)]]
        theta.prev <- pruned$theta[[res - 1]][[ceiling(elem / 4)]]
        alpha.cur <- pruned$alpha[[res]][[elem]]
        theta.cur <- pruned$theta[[res]][[elem]]
        
        ##---- Decision 1: Minimum Energy ------------------------------------##
        
        if (energy_min(theta = theta.cur, alpha = alpha.cur) >= 0.8) {
  
          pruned$status[[res]][[elem]] <- "TRUE"
          
          if (theta.cur >= 0.5) {
            pruned$matrices[[res]][[elem]][] <- 1
          } else {
            pruned$matrices[[res]][[elem]][] <- 0
          }
          
          if (res != max.res) {
            # If pruned - set further dependent statuses to TRUE
            pruned$status[[res + 1]][[1 + (elem - 1) * 4]] <- "TRUE"
            pruned$status[[res + 1]][[2 + (elem - 1) * 4]] <- "TRUE"
            pruned$status[[res + 1]][[3 + (elem - 1) * 4]] <- "TRUE"
            pruned$status[[res + 1]][[4 + (elem - 1) * 4]] <- "TRUE"
            # If pruned - set further dependent matrices to zeros
            pruned$matrices[[res + 1]][[1 + (elem - 1) * 4]][] <- 0
            pruned$matrices[[res + 1]][[2 + (elem - 1) * 4]][] <- 0
            pruned$matrices[[res + 1]][[3 + (elem - 1) * 4]][] <- 0
            pruned$matrices[[res + 1]][[4 + (elem - 1) * 4]][] <- 0
          }
        
        ##---- Decision 2: Maximum Energy ------------------------------------##
        
        } else if (energy_max(theta = theta.cur, alpha = alpha.cur) >= 0.8) {

          if (res == max.res) {
            
            # If at maximum resolution apply simulated annealing to the matrix
            pruned$matrices[[res]][[elem]] <- proposed(state = pruned$matrices[[res]][[elem]])$state
            
          } else if (res < max.res - 1) {
            
            # If at least two resolutions remain skip a resolution
            pruned$status[[res + 1]][[1 + (elem - 1) * 4]] <- "SKIP"
            pruned$status[[res + 1]][[2 + (elem - 1) * 4]] <- "SKIP"
            pruned$status[[res + 1]][[3 + (elem - 1) * 4]] <- "SKIP"
            pruned$status[[res + 1]][[4 + (elem - 1) * 4]] <- "SKIP"
            pruned$matrices[[res + 1]][[1 + (elem - 1) * 4]] <- 0
            pruned$matrices[[res + 1]][[2 + (elem - 1) * 4]] <- 0
            pruned$matrices[[res + 1]][[3 + (elem - 1) * 4]] <- 0
            pruned$matrices[[res + 1]][[4 + (elem - 1) * 4]] <- 0
            
          }
          
          if (res < max.res) {
            pruned$matrices[[res]][[elem]][] <- 0
          }
          
        ##---- Decision 3: Expected Energy -----------------------------------##
        
        } else {
          
          tau.var <- energy_var(nodes = pruned$matrices[[res]][[elem]],
                                theta = theta.prev,
                                alpha = alpha.cur)
          
          # Observed energy far lower than expected - organised structure
          if (tau.var < -2) {
            
            # Apply simulated annealing approach
            pruned$matrices[[res]][[elem]] <- proposed(state = pruned$matrices[[res]][[elem]])$state
            
            if (res < max.res) {
              # If pruned - set further dependent statuses to TRUE
              pruned$status[[res + 1]][[1 + (elem - 1) * 4]] <- "TRUE"
              pruned$status[[res + 1]][[2 + (elem - 1) * 4]] <- "TRUE"
              pruned$status[[res + 1]][[3 + (elem - 1) * 4]] <- "TRUE"
              pruned$status[[res + 1]][[4 + (elem - 1) * 4]] <- "TRUE"
              # If pruned - set further dependent matrices to zeros
              pruned$matrices[[res + 1]][[1 + (elem - 1) * 4]][] <- 0
              pruned$matrices[[res + 1]][[2 + (elem - 1) * 4]][] <- 0
              pruned$matrices[[res + 1]][[3 + (elem - 1) * 4]][] <- 0
              pruned$matrices[[res + 1]][[4 + (elem - 1) * 4]][] <- 0
            }
            
          # Observed energy lower than expected - moderatley organised structure
          } else if (tau.var < 0) {

            # Skip to the next resolution level if it exists
            if (res < max.res) {
              pruned$matrices[[res]][[elem]][] <- 0
            
            # Otherwise apply the simulated annealing approach (at max.res)
            } else {
              pruned$matrices[[res]][[elem]] <- proposed(state = pruned$matrices[[res]][[elem]])$state
            }
            
          # Observed energy larger than expected - disorganised structure  
          } else {

            # Skip two resolution levels if they exist
            if (res < max.res - 1) {
              # Current matrix set to zeros
              pruned$matrices[[res]][[elem]][] <- 0
              # Set next resolution to skip status and zero matrices
              pruned$status[[res + 1]][[1 + (elem - 1) * 4]] <- "SKIP"
              pruned$status[[res + 1]][[2 + (elem - 1) * 4]] <- "SKIP"
              pruned$status[[res + 1]][[3 + (elem - 1) * 4]] <- "SKIP"
              pruned$status[[res + 1]][[4 + (elem - 1) * 4]] <- "SKIP"
              pruned$matrices[[res + 1]][[1 + (elem - 1) * 4]] <- 0
              pruned$matrices[[res + 1]][[2 + (elem - 1) * 4]] <- 0
              pruned$matrices[[res + 1]][[3 + (elem - 1) * 4]] <- 0
              pruned$matrices[[res + 1]][[4 + (elem - 1) * 4]] <- 0
              
            # If only one more resolution exists visit it in next iteration
            } else if (res < max.res) {
              pruned$matrices[[res]][[elem]][] <- 0
              
            # If at lowest resolution apply simulated annealing approach  
            } else {
              pruned$matrices[[res]][[elem]] <- proposed(state = pruned$matrices[[res]][[elem]])$state
            }
          }
        }
        
      # Is pruned status = TRUE 
      } else if (pruned$status[[res]][[elem]] == "TRUE") {
        if (res < max.res) {
          # Then update status of leaves down the tree 
          pruned$status[[res + 1]][[1 + (elem - 1) * 4]] <- "TRUE"
          pruned$status[[res + 1]][[2 + (elem - 1) * 4]] <- "TRUE"
          pruned$status[[res + 1]][[3 + (elem - 1) * 4]] <- "TRUE"
          pruned$status[[res + 1]][[4 + (elem - 1) * 4]] <- "TRUE"
          # Then fill matrices further down the tree with zeros
          pruned$matrices[[res + 1]][[1 + (elem - 1) * 4]][] <- 0
          pruned$matrices[[res + 1]][[2 + (elem - 1) * 4]][] <- 0
          pruned$matrices[[res + 1]][[3 + (elem - 1) * 4]][] <- 0
          pruned$matrices[[res + 1]][[4 + (elem - 1) * 4]][] <- 0
        }
      }
    }
  }
  
  # Put the full matrix back together based on the pruning procedure
  
  pruned$matrices[[1]][[1]] <- matrix(0, nrow = nrow(state), ncol = ncol(state))
  
  if (max.res >= 2) {
    for (res in max.res:2) {
      for (elem in 1:(length(pruned$status[[res]]) / 4)) {
        pruned$matrices[[res - 1]][[elem]] <- rbind(
          cbind(pruned$matrices[[res]][[1 + (elem - 1) * 4]],
                pruned$matrices[[res]][[2 + (elem - 1) * 4]]),
          cbind(pruned$matrices[[res]][[3 + (elem - 1) * 4]],
                pruned$matrices[[res]][[4 + (elem - 1) * 4]])
        ) + pruned$matrices[[res - 1]][[elem]]
      }
    }
  }
  
  state <- pruned$matrices[[1]][[1]]
  
  # Calculate final probabilities
  n.tot <- neigh_tot_system(no.row = no.row, no.col = no.col)
  n.act <- neigh_act_system(nodes = state)

  energy <- energy_system(nodes = state, n.act = n.act, n.tot = n.tot)
  theta <- theta_system(nodes = state, n.act = n.act, n.tot = n.tot)
  prob <- prob_system(energy = energy, theta = theta, n.tot = n.tot)
  
  # Return Output
  output <- structure(list(state = state, status = pruned$status, prob = prob))
  
  return(output)
}