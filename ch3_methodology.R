####------------------------ Chapter 3: Methodology ------------------------####

  # Load all files in functions directory
  sapply(list.files(path = 'functions', full.names = TRUE), source)

####---- The Generalised Binomial ------------------------------------------####

  probs <- c(dgenbinom(g = seq(0, 3), pi = 0.10, theta = 1.00, size = 3),
             dgenbinom(g = seq(0, 3), pi = 0.75, theta = 0.75, size = 2),
             dgenbinom(g = seq(0, 3), pi = 0.90, theta = 0.10, size = 1))
  type <- as.factor(sort(rep(c(1:3), times = 4)))
  levels(type) <- c("G~GenBinom(0.10, 1.00, 3)", "G~GenBinom(0.75, 0.75, 2)", 
                    "G~GenBinom(0.90, 0.10, 1)")
  size <- c(rep(c(0:3), times = 3))
  
  genbinom_ex <- data.frame(probs, type, size)
  
  ggplot(genbinom_ex, aes(x = size, y = probs, fill = type)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ type) +
    labs(x = "g", y = "Probability") +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    ) +
    guides(fill = FALSE) +
    geom_hline(yintercept = 0, lty = 1, colour = "lightgray", size = 0.5) +
    coord_cartesian(ylim = c(0, 1))

####---- 2.3 Statistical Properties of the Lattice Energy ------------------####

#### 2.3.1 2 x 2 Lattice ####
  
  N <- 2
  M <- 2
  theta <- seq(from = 0, to = 1, by = 1 / (M * N))
  
  analytic_exp <- expected_value(theta = theta, N = N, M = M)
  analytic_var_nodes <- variance_nodes_sum(theta = theta, N = N, M = M) +
    4 * covariance_c1(theta)
  analytic_var_struct <- variance_nodes_sum(theta = theta, N = N, M = M) +
    8 * covariance_s1(theta, n1 = 1, n2 = 1) +
    4 * covariance_s3(theta)
    
  sims <- energy_sims(N = N, M = M)
  numeric_exp <- as.vector(by(sims$E, sims$p, mean, na.rm = TRUE))
  numeric_var <- as.vector(by(sims$E, sims$p, var, na.rm = TRUE))
  
  results <- data.frame(theta, analytic_exp, numeric_exp, analytic_var_nodes, 
                        analytic_var_struct, numeric_var)
  
  theta <- c(0.25, 0.5, 0.75)
  energies <- c(-8, 0, +8)
  ex_2x2 <- data.frame(
    theta = sort(rep(theta, times = length(energies))),
    energies = rep(energies, times = length(theta)),
    probability = NA
  )
  
  for (i in 1:nrow(ex_2x2)) {
    ex_2x2[i, "probability"] <- energy_pdf_2x2(energy = ex_2x2$energies[i],
                                               theta = ex_2x2$theta[i])
  }
  
  ggplot(ex_2x2, aes(x = energies, y = probability, fill = theta)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ theta) +
    labs(x = "Energy", y = "Probability") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    guides(fill = FALSE) +
    geom_hline(yintercept = 0, lty = 1, colour = "lightgray", size = 0.5)
  
#### 2.3.2 2 x 3 Lattice ####

  N <- 2
  M <- 3
  theta <- seq(from = 0, to = 1, by = 1 / (M * N))
  
  analytic_exp <- expected_value(theta = theta, N = N, M = M)
  analytic_var_nodes <- variance_nodes_sum(theta = theta, N = N, M = M) +
    4 * covariance_c2(theta) +
    2 * covariance_e1(theta)
  analytic_var_struct <- variance_nodes_sum(theta = theta, N = N, M = M) +
    4 * covariance_s1(theta, n1 = 1, n2 = 1) +
    8 * covariance_s1(theta, n1 = 1, n2 = 2) +
    2 * covariance_s1(theta, n1 = 2, n2 = 2) +
    4 * covariance_s2(theta) +
    8 * covariance_s3(theta) 
  
  sims <- energy_sims(N = N, M = M)
  numeric_exp <- as.vector(by(sims$E, sims$p, mean, na.rm = TRUE))
  numeric_var <- as.vector(by(sims$E, sims$p, var, na.rm = TRUE))

  results <- data.frame(theta, analytic_exp, numeric_exp, analytic_var_nodes, 
                        analytic_var_struct, numeric_var)

#### 2.3.3 2 x M Lattice ####
  
  N <- 2
  M <- 7
  theta <- seq(from = 0, to = 1, by = 1 / (M * N))
  
  analytic_exp <- expected_value(theta = theta, N = N, M = M)
  analytic_var_nodes <- variance_nodes_sum(theta = theta, N = N, M = M) +
    4 * covariance_c2(theta) +
    4 * covariance_e2(theta) +
    (2 * M - 8) * covariance_e3(theta)
  analytic_var_struct <- variance_nodes_sum(theta = theta, N = N, M = M) +
    4 * covariance_s1(theta, n1 = 1, n2 = 1) +
    8 * covariance_s1(theta, n1 = 1, n2 = 2) +
    (6 * M - 16) * covariance_s1(theta, n1 = 2, n2 = 2) +
    (4 * M - 8) * covariance_s2(theta) +
    (4 * M - 4) * covariance_s3(theta) 
  
  sims <- energy_sims(N = N, M = M)
  numeric_exp <- as.vector(by(sims$E, sims$p, mean, na.rm = TRUE))
  numeric_var <- as.vector(by(sims$E, sims$p, var, na.rm = TRUE))
  
  results <- data.frame(theta, analytic_exp, numeric_exp, analytic_var_nodes, 
                        analytic_var_struct, numeric_var)
  
#### 2.3.4 3 x 3 Lattice ####
  
  N <- 3
  M <- 3
  theta <- seq(from = 0, to = 1, by = 1 / (M * N))
  
  analytic_exp <- expected_value(theta = theta, N = N, M = M)
  analytic_var_nodes <- variance_nodes_sum(theta = theta, N = N, M = M) +
    4 * covariance_c3(theta) +
    4 * covariance_e2(theta) +
    1 * covariance_i1(theta)
  analytic_var_struct <- variance_nodes_sum(theta = theta, N = N, M = M) +
    16 * covariance_s1(theta, n1 = 1, n2 = 2) +
    8 * covariance_s1(theta, n1 = 2, n2 = 3) +
    12 * covariance_s2(theta) +
    16 * covariance_s3(theta)
  
  sims <- energy_sims(N = N, M = M)
  numeric_exp <- as.vector(by(sims$E, sims$p, mean, na.rm = TRUE))
  numeric_var <- as.vector(by(sims$E, sims$p, var, na.rm = TRUE))
  
  results <- data.frame(theta, analytic_exp, numeric_exp, analytic_var_nodes, 
                        analytic_var_struct, numeric_var)
  
  theta <- c(0.25, 0.5, 0.75)
  energies <- c(-24, -16, -12, -8, -4, 0, +4, +8, +12, +16, +24)
  ex_3x3 <- data.frame(
    theta = sort(rep(theta, times = length(energies))),
    energies = rep(energies, times = length(theta)),
    probability = NA
  )
  
  for (i in 1:nrow(ex_3x3)) {
    ex_3x3[i, "probability"] <- energy_pdf_3x3(energy = ex_3x3$energies[i],
                                               theta = ex_3x3$theta[i])
  }

  ggplot(ex_3x3, aes(x = energies, y = probability, fill = theta)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ theta) +
    labs(x = "Energy", y = "Probability") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    guides(fill = FALSE) +
    geom_hline(yintercept = 0, lty = 1, colour = "lightgray", size = 0.5)
  
## 2.3.5 3 x M Lattice
  
  N <- 3
  M <- 10
  theta <- seq(from = 0, to = 1, by = 1 / (M * N))
  
  analytic_exp <- expected_value(theta = theta, N = N, M = M)
  analytic_var_nodes <- variance_nodes_sum(theta = theta, N = N, M = M) +
    4 * covariance_c3(theta) +
    2 * covariance_e2(theta) +
    4 * covariance_e3(theta) +
    (2 * M - 8) * covariance_e4(theta) +
    2 * covariance_i2(theta) +
    (M - 4) * covariance_i3(theta)
  analytic_var_struct <- variance_nodes_sum(theta = theta, N = N, M = M) +
    16 * covariance_s1(theta, n1 = 1, n2 = 2) +
    (4 * M - 12) * covariance_s1(theta, n1 = 2, n2 = 2) +
    (4 * M - 4) * covariance_s1(theta, n1 = 2, n2 = 3) +
    (2 * M - 6) * covariance_s1(theta, n1 = 3, n2 = 3) +
    (8 * M - 12) * covariance_s2(theta) +
    (8 * M - 8) * covariance_s3(theta)
  
  sims <- energy_sims(N = N, M = M)
  numeric_exp <- as.vector(by(sims$E, sims$p, mean, na.rm = TRUE))
  numeric_var <- as.vector(by(sims$E, sims$p, var, na.rm = TRUE))
  
  results <- data.frame(theta, analytic_exp, numeric_exp, analytic_var_nodes, 
                        analytic_var_struct, numeric_var)
  
#### 2.3.5 N x M Lattice ####
  
  N <- 5
  M <- 5
  theta <- seq(from = 0, to = 1, by = 1 / (M * N))
  
  analytic_exp <- expected_value(theta = theta, N = N, M = M)
  analytic_var_nodes <- variance_nodes_sum(theta = theta, N = N, M = M) +
    4 * covariance_c3(theta) +
    8 * covariance_e3(theta) +
    (2 * (N + M) - 16) * covariance_e4(theta) +
    4 * covariance_i3(theta) +
    (2 * (N + M) - 16) * covariance_i4(theta) +
    (N - 4) * (M - 4) * covariance_i5(theta)
  analytic_var_struct <- variance_nodes_sum(theta = theta, N = N, M = M) +
    16 * covariance_s1(theta, n1 = 1, n2 = 2) +
    (4 * N + 4 * M - 24) * covariance_s1(theta, n1 = 2, n2 = 2) +
    (4 * N + 4 * M - 16) * covariance_s1(theta, n1 = 2, n2 = 3) +
    (4 * N * M - 10 * N - 10 * M + 24) * covariance_s1(theta, n1 = 3, n2 = 3) +
    (4 * N * M - 4 * N - 4 * M) * covariance_s2(theta) +
    (4 * N * M - 4 * N - 4 * M + 4) * covariance_s3(theta)
  
  sims <- energy_sims(N = N, M = M)
  numeric_exp <- as.vector(by(sims$E, sims$p, mean, na.rm = TRUE))
  numeric_var <- as.vector(by(sims$E, sims$p, var, na.rm = TRUE))
  
  results <- data.frame(theta, analytic_exp, numeric_exp, analytic_var_nodes,
                        analytic_var_struct, numeric_var)
  
####---- 2.4 Probabilistic Approximation of the Lattice Energy -------------####
  
####---- 2.5 Properties of the Estimands -----------------------------------####

#### Heatmap of Energy Function ####
  
  N <- 3
  M <- 3
  theta <- seq(0, 1, by = 0.02)
  mu <- -1 * (4 * N * M - 2 * N - 2 * M) * (2 * theta - 1) ^ 2
  variance <- theta * (1 - theta) * 
    ((224 * N * M - 208 * N - 208 * M + 128) * (2 * theta - 1) ^ 2 + 
       (32 * N * M - 16 * N - 16 * M))
  bnd.2.5p <- energy_quant_3x3(prob = 0.025, theta = theta)
  bnd.5p <- energy_quant_3x3(prob = 0.05, theta = theta)
  bnd.25p <- energy_quant_3x3(prob = 0.25, theta = theta)
  bnd.75p <- energy_quant_3x3(prob = 0.75, theta = theta)
  bnd.95p <- energy_quant_3x3(prob = 0.95, theta = theta)
  bnd.97.5p <- energy_quant_3x3(prob = 0.975, theta = theta)
  
  no.runs <- 10000
  sims <- energy_sims(N = 3, M = 3, no.runs = no.runs, prop = theta)
  energies <- data.frame("energy" = seq(-24, 24, by = 4))
  for (i in 1:length(unique(sims$p))) {
    tab <- factor(sims$E[sims$p == unique(sims$p)[i]], 
                  levels = energies$energy)
    energies[i + 1] <- table(tab)# / no.runs
  }
  names(energies)[2:ncol(energies)] <- theta
  energies <- as.data.frame(energies)
  energies$energy <- factor(energies$energy, levels = seq(-24, 24, by = 4))
  energies <- melt(energies)
  energies <- energies[, c(1, 2, 4)]
  names(energies) <- c("energy", "variable", "value")
  energies$value[energies$value == 0] <- NA

  ggplot() + 
    geom_tile(data = energies, aes(variable, energy, fill = value), 
              colour = "white") + 
    scale_fill_gradient(low = "gray90", high = "steelblue", 
                         guide = FALSE, na.value = "white") + 
    theme_bw(base_size = 11) + 
    labs(x = expression(theta), y = expression(xi[L])) + 
    scale_x_discrete(expand = c(0, 0),
                     breaks = theta[seq(1, length(theta), by = 5)]) +
    scale_y_discrete(expand = c(0, 0)) +
    theme(legend.position = "right") +
    geom_line(aes(x = 1:length(mu), 
                  y = (mu + 24) / 4 + 1), 
              color = "red", size = 0.8) + 
    geom_line(aes(x = 1:length(bnd.5p), 
                  y = (bnd.5p + 24) / 4 + 1), 
              color = "blue", size = 0.8, lty = 2) +
    geom_line(aes(x = 1:length(bnd.95p), 
                  y = (bnd.95p + 24) / 4 + 1), 
              color = "blue", size = 0.8, lty = 2) + 
    geom_line(aes(x = 1:length(bnd.2.5p), 
                  y = (bnd.2.5p + 24) / 4 + 1), 
              color = "purple", size = 0.8, lty = 3) + 
    geom_line(aes(x = 1:length(bnd.97.5p), 
                  y = (bnd.97.5p + 24) / 4 + 1), 
              color = "purple", size = 0.8, lty = 3) +
    geom_line(aes(x = 1:length(bnd.75p), 
                  y = (bnd.25p + 24) / 4 + 1), 
              color = "forestgreen", size = 0.8, lty = 4) +
    geom_line(aes(x = 1:length(bnd.75p), 
                  y = (bnd.75p + 24) / 4 + 1), 
              color = "forestgreen", size = 0.8, lty = 4)
  
#### Variance of the Energy Function ####

  no.samples <- 10000
  no.runs <- 1000
  sims.theta <- seq(0, 1, by = 0.01)
  sims.var <- matrix(0, ncol = length(sims.theta), nrow = no.samples)
  sims.1per <- matrix(0, ncol = length(sims.theta), nrow = no.samples)
  sims.5per <- matrix(0, ncol = length(sims.theta), nrow = no.samples)
  sims.10per <- matrix(0, ncol = length(sims.theta), nrow = no.samples)
  sims.25per <- matrix(0, ncol = length(sims.theta), nrow = no.samples)
  sims.75per <- matrix(0, ncol = length(sims.theta), nrow = no.samples)
  sims.90per <- matrix(0, ncol = length(sims.theta), nrow = no.samples)
  sims.95per <- matrix(0, ncol = length(sims.theta), nrow = no.samples)
  sims.99per <- matrix(0, ncol = length(sims.theta), nrow = no.samples)
  
  for (i in 1:no.samples) {
    sims <- energy_sims(N = N, M = M, no.runs = no.runs, prop = sims.theta)
    
    sims.var[i, ] <- as.vector(by(sims$E, sims$p, var, na.rm = TRUE))
    sims.1per[i, ] <- as.vector(by(sims$E, sims$p, quantile, probs = 0.01))
    sims.5per[i, ] <- as.vector(by(sims$E, sims$p, quantile, probs = 0.05))
    sims.10per[i, ] <- as.vector(by(sims$E, sims$p, quantile, probs = 0.10))
    sims.25per[i, ] <- as.vector(by(sims$E, sims$p, quantile, probs = 0.25))
    sims.75per[i, ] <- as.vector(by(sims$E, sims$p, quantile, probs = 0.75))
    sims.90per[i, ] <- as.vector(by(sims$E, sims$p, quantile, probs = 0.90))
    sims.95per[i, ] <- as.vector(by(sims$E, sims$p, quantile, probs = 0.95))
    sims.99per[i, ] <- as.vector(by(sims$E, sims$p, quantile, probs = 0.99))
  }
  
  sims.var <- as.vector(sims.var)
  sims.1per <- as.vector(sims.1per)
  sims.5per <- as.vector(sims.5per)
  sims.10per <- as.vector(sims.10per)
  sims.25per <- as.vector(sims.25per)
  sims.75per <- as.vector(sims.75per)
  sims.90per <- as.vector(sims.90per)
  sims.99per <- as.vector(sims.99per)
  sims.theta <- sort(rep(sims.theta, times = no.samples))
  
  ggplot() + 
    theme_bw() +
    labs(x = expression(theta), y = expression(sigma^2)) +
    geom_boxplot(aes(x = sims.theta, y = sims.var, group = sims.theta),
                 colour = "steelblue") +
    geom_line(aes(x = theta, y = variance), size = 0.8, colour = "red") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())