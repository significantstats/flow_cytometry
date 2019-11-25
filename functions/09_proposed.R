proposed <- function(state, temp = 4) {
  
  # Dimensions of input matrix
  no.row <- nrow(state)
  no.col <- ncol(state)
  no.elements <- length(state)
  
  # Simulated Annealing Controls
  SA.stop <- FALSE
  SA.update <- FALSE
  SA.updates <- 0
  SA.updates.max <- 43
  MH.runs <- 0
  MH.runs.max <- 7500
  temp.cur <- temp
  MH.total <- 0
  MH.accept.max <- 0.1 * no.elements
  
  # Holding matrices
  state.cur <- state
  n.tot <- neigh_tot_system(no.row, no.col)
  n.act.cur <- neigh_act_system(state.cur)
  n.tot.ext <- neigh_extend_tot_system(no.row, no.col)
  n.act.ext.cur <- neigh_extend_act_system(state.cur)
  energy.cur <- energy_system(state.cur, n.act.cur, n.tot)
  theta.cur <- theta_system(state.cur, n.act.ext.cur, n.tot.ext)
  prob.cur <- prob_system(energy.cur, theta.cur, n.tot)
  
  while (SA.stop != TRUE) {
    
    temp.cur <- temp.cur * (0.95 ^ SA.updates)
    SA.energy <- energy.cur
    SA.updates <- SA.updates + 1
    SA.update <- FALSE
    MH.total <- MH.total + MH.runs
    MH.runs <- 0
    MH.accepted <- 0
    
    while (SA.update != TRUE) {
      
      # Pick a node based on its node probability
      index <- sample(1:no.elements, size = 1, 
                      prob = (1 / prob.cur) / (sum(1 / prob.cur)))
      
      # Systme state when node is changed
      state.swap <- state.cur
      state.swap[index] <- 1 - state.swap[index]
      
      # System active neighbours when node is changed
      n.act.swap <- neigh_act_update(n.act.cur, state.swap, index)
      n.act.ext.swap <- neigh_extend_act_update(n.act.ext.cur, state.swap, index)
      
      # System energy when node is changed
      energy.swap <- energy_update(energy.cur, index, n.act.swap, 
                                   state.swap, n.tot)
      
      # Difference in system energies (pre- and post-change)
      energy.change <- energy_change(energy.cur, energy.swap, index)
      
      # Metropolis Criterion
      if (energy.change >= 0) {
        
        state.cur <- state.swap
        n.act.cur <- n.act.swap
        n.act.ext.cur <- n.act.ext.swap
        energy.cur <- energy.swap
        theta.cur <- theta_update(theta.cur, state.cur, n.act.ext.cur, n.tot.ext, index)
        prob.cur <- prob_update(prob.cur, energy.cur, theta.cur, n.tot, index)
        MH.accepted <- MH.accepted + 1
        
      } else {
        if (exp(energy.change / temp.cur) > runif(1)) {
          
          state.cur <- state.swap
          n.act.cur <- n.act.swap
          n.act.ext.cur <- n.act.ext.swap
          energy.cur <- energy.swap
          theta.cur <- theta_update(theta.cur, state.cur, n.act.ext.cur, n.tot.ext, index)
          prob.cur <- prob_update(prob.cur, energy.cur, theta.cur, n.tot, index)
          MH.accepted <- MH.accepted + 1
          
        }
      }
      
      MH.runs <- MH.runs + 1
      if (MH.runs >= MH.runs.max || MH.accepted >= MH.accept.max) {
        SA.update <- TRUE
      }
    }
    
    # Are we finished annealing?
    if (sum(energy.cur - SA.energy) / sum(SA.energy) < 0.001 || SA.updates >= SA.updates.max) {
      SA.stop <- TRUE
    }
  }
  
  output <- structure(list(state = state.cur, prob = prob.cur, temp = temp.cur,
                           total.iter = MH.total, SA.updates = SA.updates))
  
  return(output)
}