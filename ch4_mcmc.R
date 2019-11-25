####--------------------- Chapter 3: Accelerated MCMC ----------------------####

  # Load all files in functions directory
  sapply(list.files(path = 'functions', full.names = TRUE), source)

####---- 3.1 Existing Approaches -------------------------------------------####

####---- 3.2 Metropolis-Hasting --------------------------------------------####

####---- 3.3 Simualted Annealling ------------------------------------------####

####---- 3.4 Proposed Adaptation -------------------------------------------####
  
  # (a) all nodes in agreement (+1s)
  X <- matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1), nrow = 3, ncol = 3)

  n.tot <- neigh_tot_system(no.row = nrow(X), no.col = ncol(X))
  n.act <- neigh_act_system(nodes = X)
  
  n.tot.ext <- neigh_extend_tot_system(no.row = nrow(X), no.col = ncol(X))
  n.act.ext <- neigh_extend_act_system(nodes = X)
  
  energy <- energy_system(nodes = X, n.act = n.act, n.tot = n.tot)
  theta <- theta_system(nodes = X, n.act = n.act.ext, n.tot = n.tot.ext)
  
  prob <- prob_system(energy = energy, theta = theta, n.tot = n.tot)
  
  P <- (1 / prob) / sum(1 / prob)
  
  # (b) all nodes in agreement (+1s) bar the middle node
  X <- matrix(c(1, 1, 1, 1, 0, 1, 1, 1, 1), nrow = 3, ncol = 3)
  
  n.tot <- neigh_tot_system(no.row = nrow(X), no.col = ncol(X))
  n.act <- neigh_act_system(nodes = X)
  
  n.tot.ext <- neigh_extend_tot_system(no.row = nrow(X), no.col = ncol(X))
  n.act.ext <- neigh_extend_act_system(nodes = X)
  
  energy <- energy_system(nodes = X, n.act = n.act, n.tot = n.tot)
  theta <- theta_system(nodes = X, n.act = n.act.ext, n.tot = n.tot.ext)
  
  prob <- prob_system(energy = energy, theta = theta, n.tot = n.tot)
  
  P <- (1 / prob) / sum(1 / prob)
  
  # (c) all nodes in disagreement with their neighbours
  X <- matrix(c(1, 0, 1, 0, 1, 0, 1, 0, 1), nrow = 3, ncol = 3)
  
  n.tot <- neigh_tot_system(no.row = nrow(X), no.col = ncol(X))
  n.act <- neigh_act_system(nodes = X)
  
  n.tot.ext <- neigh_extend_tot_system(no.row = nrow(X), no.col = ncol(X))
  n.act.ext <- neigh_extend_act_system(nodes = X)
  
  energy <- energy_system(nodes = X, n.act = n.act, n.tot = n.tot)
  theta <- theta_system(nodes = X, n.act = n.act.ext, n.tot = n.tot.ext)
  
  prob <- prob_system(energy = energy, theta = theta, n.tot = n.tot)
  
  P <- (1 / prob) / sum(1 / prob)

####---- 3.5 Application to Binary Images ----------------------------------####
  
####---- 3.5.1 Letter A ----------------------------------------------------####
  
    A <- read.bmp('data/lettera.bmp')
    A[A != 29] <- 1
    A[A == 29] <- 0
    A <- t(A[rev(1:nrow(A)), ])
    
    # Plot of the letter A
    image_ggplot(A)
    
    # Add noise to the letter
    A.noise <- image_distort(A, 0.2)
    image_ggplot(A.noise)
    
    # Simple Simulated Annealing Approach
    a.anneal.time <- Sys.time()
    A.anneal <- sim_anneal(state = A.noise, temp = 4)
    a.anneal.time <- Sys.time() - a.anneal.time
    image_ggplot(A.anneal$state)
    
    # Adapted Simulated Annealing Approach
    a.proposed.time <- Sys.time()
    A.proposed <- proposed(state = A.noise, temp = 4)
    a.proposed.time <- Sys.time() - a.proposed.time
    image_ggplot(A.proposed$state)
    
    image(A.proposed$prob)
    
    # Table of results 
    orig.anneal <- table(A, A.anneal$state)
    orig.prop <- table(A, A.proposed$state)
    anneal.prop <- table(A.anneal$state, A.proposed$state)
        
####---- 3.5.2 Letter G ----------------------------------------------------####
    
    G <- read.bmp('data/gt.bmp')[ , , 3]
    G[G != 245] <- 1
    G[G == 245] <- 0
    G <- t(G[rev(1:nrow(G)), ])
    
    # Plot of the letter G
    image_ggplot(G)
    
    # Add noise to the letter
    G.noise <- image_distort(G, 0.2)
    image_ggplot(G.noise)
    
    # Simple Simulated Annealing Approach
    g.anneal.time <- Sys.time()
    G.anneal <- sim_anneal(state = G.noise, temp = 4)
    g.anneal.time <- Sys.time() - g.anneal.time
    image_ggplot(G.anneal$state)

    # Adapted Simulated Annealing Approach
    g.proposed.time <- Sys.time()
    G.proposed <- proposed(state = G.noise, temp = 4)
    g.proposed.time <- Sys.time() - g.proposed.time
    image_ggplot(G.proposed$state)

    # Table of results 
    orig.anneal <- table(G, G.anneal$state)
    orig.prop <- table(G, G.proposed$state)
    anneal.prop <- table(G.anneal$state, G.proposed$state)