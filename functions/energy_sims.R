energy_sims <- function(N, M, no.runs = NULL, prop = NULL) {

  n <- N
  m <- M

  if (is.null(no.runs)) {
    no.runs <- floor(0.1 * (2 ^ 31 - 21) / (5 * N * M - 5))
  }
  
  if (is.null(prop)) {
    prop <- seq(from = 0, to = 1, by = 1 / (n * m))
    sims.out <- matrix(NA, ncol = 5, nrow = (length(prop) * no.runs))
  } else {
    sims.out <- matrix(NA, ncol = 5, nrow = (length(prop) * no.runs))
  }
  
  count <- 0

  # Set up a progress bar
  print(paste0("Simulations ", N, " x ", M, " Lattice Running:"))
  pb <- txtProgressBar(style = 3, min = count, max = nrow(sims.out))  
  
  for (p in prop) {
    for (sims in 1:no.runs) {
      count <- count + 1
      config <- sample(c(0, 1), prob = c(1 - p, p), size = n * m,
                       replace = TRUE)
      config <- matrix(config, nrow = n, ncol = m)

      occupancy <- sum(config) / (n * m)
      energy <- energy(config, neigh.sys = "VN1")

      sims.out[count, ] <- c(n, m, p, occupancy, energy)
      
      if (count %% 100 == 0) {
        setTxtProgressBar(pb, value = count)
      }
    }
  }

  sims.out <- as.data.frame(sims.out)
  names(sims.out) <- c("N", "M", "p", "occ", "E")

  close(pb)

  return(sims.out)
}