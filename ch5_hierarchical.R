####---------------------- Multi-resolution Analysis -----------------------####

  # Load all files in functions directory
  sapply(list.files(path = 'functions', full.names = TRUE), source)

####---- 4.1 Multi-Resolution Methods --------------------------------------####

####---- 4.2 Pruning -------------------------------------------------------####

  # Decision Tree & Pruned Decision Tree
  titanic <- read.csv(file = 'data/titanic.csv')
  
  dec.tree <- rpart(Survived ~ Pclass + Sex + Age, 
                    data = titanic, method = "class",
                    control = rpart.control(minsplit = 2, cp = 0))
  
  dec.tree.prune <- rpart(Survived ~ Pclass + Sex + Age,
                          data = titanic, method = "class")

  fancyRpartPlot(dec.tree, sub = "")
  
  fancyRpartPlot(dec.tree.prune, sub = "")
  
####---- 4.3 Dyadic Structures ---------------------------------------------####

####---- 4.4 Proposed Method -----------------------------------------------####

  # Decision 1: Minimum Energy
  N <- 2 ^ seq(from = 1, by = 1, to = 6)
  M <- 2 ^ seq(from = 1, by = 1, to = 6)
  alpha <- N * M
  theta <- seq(from = 0, by = 1 / max(alpha), to = 1)
  
  energy.min <- data.frame(
    alpha = sort(rep(alpha, times = length(theta))),
    theta = rep(theta, times = length(alpha)),
    min = NA
  )
  
  energy.min$min <- energy_min(theta = energy.min$theta,
                               alpha = energy.min$alpha)
  
  ggplot(data = energy.min, aes(x = theta, y = min)) +
    geom_line(aes(group = alpha, colour = as.factor(alpha))) +
    labs(x = expression(theta), 
         y = expression("Pr(min "*xi*")"),
         colour = "Lattice Size") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    guides(colour = FALSE)
  
  # Decision 2: Maximum Energy
  N <- 2 ^ seq(from = 1, by = 1, to = 6)
  M <- 2 ^ seq(from = 1, by = 1, to = 6)
  alpha <- N * M
  theta <- seq(from = 0.01, by = 0.01, to = 0.99)
  
  energy.max <- data.frame(
    alpha = sort(rep(alpha, times = length(theta))),
    theta = rep(theta, times = length(alpha)),
    max = NA
  )
  
  energy.max$max <- energy_max(theta = energy.max$theta,
                               alpha = energy.max$alpha)
  
  ggplot(data = energy.max, aes(x = theta, y = max)) +
    geom_line(aes(group = alpha, colour = as.factor(alpha))) +
    labs(x = expression(theta), 
         y = expression("Pr(max "*xi*")"),
         colour = "Lattice Size") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    guides(colour = FALSE)
  
  # Decision 3: Expected Energy
  X1 <- matrix(c(1, 1, 0, 0,
                 1, 1, 0, 0,
                 1, 1, 0, 0,
                 1, 1, 0, 0), byrow = TRUE, ncol = 4)
  energy_var(nodes = X1, alpha = 16, theta = 0.5)
  
  X2 <- matrix(c(1, 1, 0, 0,
                 1, 1, 0, 0,
                 0, 0, 1, 1,
                 0, 0, 1, 1), byrow = TRUE, ncol = 4)
  energy_var(nodes = X2, alpha = 16, theta = 0.5)
  
  X3 <- matrix(c(1, 1, 1, 1,
                 0, 1, 1, 1,
                 0, 0, 0, 0,
                 0, 1, 0, 0), byrow = TRUE, ncol = 4)
  energy_var(nodes = X3, alpha = 16, theta = 0.5)
  
  X4 <- matrix(c(1, 0, 0, 1,
                 0, 1, 1, 0,
                 1, 0, 0, 1,
                 0, 1, 1, 0), byrow = TRUE, ncol = 4)
  energy_var(nodes = X4, alpha = 16, theta = 0.5)
  
####---- 4.5 Examples ------------------------------------------------------####
  
####---- 4.5.1 Letter A ----------------------------------------------------####
  
  A <- read.bmp('data/lettera.bmp')
  A[A != 29] <- 1
  A[A == 29] <- 0
  A <- t(A[rev(1:nrow(A)), ])
  
  # Plot of the letter A
  image_ggplot(A)
  
  # Add noise to the letter
  A.noise <- image_distort(A, 0.2)
  image_ggplot(A.noise)
  
  # Adapted Simulated Annealing Approach
  A.ch3.time <- Sys.time()
  A.ch3 <- proposed(state = A.noise, temp = 4)
  A.ch3.time <- Sys.time() - A.ch3.time
  
  image_ggplot(A.ch3$state)
  
  # Hierarchical Approach
  A.ch4.time <- Sys.time()
  A.ch4 <- hierarchical(state = A.noise, temp = 4)
  A.ch4.time <- Sys.time() - A.ch4.time

  image_ggplot(A.ch4$state)

  # Comparison between Simulated Annealing & Hierarchical Approach
  orig.mcmc <- table(A, A.ch3$state)
  orig.multires <- table(A, A.ch4$state)
  
####---- 4.5.2 Letter G ----------------------------------------------------####  
  
  G <- read.bmp('data/gt.bmp')[ , , 3]
  G[G != 245] <- 1
  G[G == 245] <- 0
  G <- t(G[rev(1:nrow(G)), ])
  
  # Plot of the letter G
  image_ggplot(G)
  
  # Add noise to the letter
  G.noise <- image_distort(G, 0.2)
  image_ggplot(G.noise)
  
  # Adapted Simulated Annealing Approach
  G.ch3.time <- Sys.time()
  G.ch3 <- sim_anneal(state = G.noise, temp = 4)
  G.ch3.time <- Sys.time() - G.ch3.time

  image_ggplot(G.ch3$state)
  
  # Hierarchical Approach
  G.ch4.time <- Sys.time()
  G.ch4 <- hierarchical(state = G.noise, temp = 4)
  G.ch4.time <- Sys.time() - G.ch4.time
  
  image_ggplot(G.ch4$state)
  
  # Comparison between Simulated Annealing & Hierarchical Approach
  orig.mcmc <- table(G, G.ch3$state)
  orig.multires <- table(G, G.ch4$state)