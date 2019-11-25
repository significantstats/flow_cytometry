####---------------------- Energy PDF - Exact Solution ---------------------####

energy_pdf_2x2 <- Vectorize( function(energy, theta) {
  
  p <- theta
  
  if (energy == -8) {
    prob <- p ^ 4 + (1 - p) ^ 4
  } else if (energy == 0) {
    prob <- 4 * p * (1 - p) * (p ^ 2 - p + 1)
  } else if (energy == 8) {
    prob <- 2 * p ^ 2 * (1 - p) ^ 2
  } else {
    prob <- 0
  }
  
  return(prob)
}, vectorize.args = "energy")

energy_cdf_2x2 <- function(energy, theta) {
  
  if (energy < -8) {
    prob <- 0
  } else if (energy >= 8) {
    prob <- 1
  } else {
    energies <- c(-8, 0, 8)
    prob <- 0
    for (i in 1:length(energies)) {
      if (energy >= energies[i]) {
        prob <- prob + energy_pdf_2x2(energy = energies[i], theta = theta)
      }
    }
  }
  
  return(prob)
}

energy_quant_2x2 <- Vectorize( function(prob, theta) {
  
  if (prob <= 0) {
    quant <- -8
  } else if (prob >= 1) {
    quant <- 8
  } else {
    energies <- c(-8, 0, 8)
    cdf <- 0
    for (i in 1:length(energies)) {
      cdf <- energy_cdf_2x2(energy = energies[i], theta = theta)
      if (cdf > prob) {
        quant <- energies[i]
        break
      }
    }
  }
  
  return(quant)
}, vectorize.args = "theta")

energy_pdf_3x3 <- Vectorize( function(energy, theta) {
  
  p <- theta
  
  if (energy == -24) {
    prob <- p ^ 9 + (1 - p) ^ 9
  } else if (energy == -16) {
    prob <- 4 * p * (1 - p) * ((1 - p) ^ 7 + p ^ 7)
  } else if (energy == -12) {
    prob <- 4 * p * (1 - p) * ((1 - p) ^ 5 + p ^ 5)
  } else if (energy == -8) {
    prob <- -23 * p ^ 6 + 69 * p ^ 5 - 81 * p ^ 4 + 47 * p ^ 3 - 11 * p ^ 2 - p + 1
    prob <- p * (1 - p) * prob
  } else if (energy == -4) {
    prob <- 12 * p ^ 2 * (1 - p) ^ 2 * (3 * p ^ 2 - 3 * p + 1)
  } else if (energy == 0) {
    prob <- 2 * p ^ 2 * (1 - p) ^ 2 * (4 * p ^ 4 - 8 * p ^ 3 + 16 * p ^ 2 - 12 * p + 5)
  } else if (energy == 4) {
    prob <- 12 * p ^ 3 * (1 - p) ^ 3
  } else if (energy == 8) {
    prob <- p ^ 3 * (1 - p) ^ 3 * (17 * p ^ 2 - 17 * p + 10)
  } else if (energy == 12) {
    prob <- 4 * p ^ 3 * (1 - p) ^ 3
  } else if (energy == 16) {
    prob <- 4 * p ^ 4 * (1 - p) ^ 4
  } else if (energy == 24) {
    prob <- p ^ 4 * (1 - p) ^ 4
  } else {
    prob <- 0
  }
  
  return(prob)
}, vectorize.args = c("energy"))

energy_cdf_3x3 <- function(energy, theta) {
  
  if (energy < -24) {
    prob <- 0
  } else if (energy >= 24) {
    prob <- 1
  } else {
    energies <- c(-24, seq(-16, 16, by = 4), 24)
    prob <- 0
    for (i in 1:length(energies)) {
      if (energy >= energies[i]) {
        prob <- prob + energy_pdf_3x3(energy = energies[i], theta = theta)
      }
    }
  }
  
  return(prob)
}

energy_quant_3x3 <- Vectorize( function(prob, theta) {
  
  if (prob <= 0) {
    quant <- -24
  } else if (prob >= 1) {
    quant <- 24
  } else {
    energies <- c(-24, seq(-16, 16, by = 4), 24)
    cdf <- 0
    for (i in 1:length(energies)) {
      cdf <- energy_cdf_3x3(energy = energies[i], theta = theta)
      if (cdf > prob) {
        quant <- energies[i]
        break
      }
    }
  }
  
  return(quant)
}, vectorize.args = "theta")