####----------------------------- Introduction -----------------------------####

  # Load all files in functions directory
  sapply(list.files(path = 'functions', full.names = TRUE), source)
  
  # Load the Rituximab Data
  rit <- read.csv(file = 'data/rituximab/rituximab.csv')

  # Set up the layout matrix for the graphics
  par(pty = "s")
  nf <- layout(matrix(c(1, 3, 5, 2, 4, 6), nrow = 3, ncol = 2))
  
  # Plot 1: Two FCM Variables
  plot(x = rit$FSC.H, y = rit$SSC.H, xlab = "FSC (Height)", las = 1,
       ylab = "SSC (Height)", main = "(a) Original Rituximab Data", cex = 0.2)

  # Plot 2: Subjective Manual Gating
  plot(x = rit$FSC.H, y = rit$SSC.H, xlab = "FSC (Height)", las = 1,
       ylab = "SSC (Height)", main = "(b) Manual Gating", cex = 0.2)
  polygon(x = c(50, 50, 440, 380), y = c(0, 200, 580, 80),
          border = "red", lwd = 2, lty = 1)
  polygon(x = c(50, 50, 420, 600), y = c(0, 200, 610, 110),
          border = "blue", lwd = 2, lty = 1)
  
  # Plot 3: Lo's Approach
  lo.rit <- flowClust(x = rit, varNames = c("FSC.H", "SSC.H"), K = 1)
  plot(lo.rit, data = rit, xlab = "FSC (Height)", 
       ylab = "SSC (Height)", las = 1, 
       main = "(c) Lo et al. (2008) Approach", show.outliers = TRUE,
       pch.outliers = 20, cex = 1.5, cex.outliers = 0.4)

  # Plot 4: Lattice Structure of FCM Data
  plot(x = rit$FSC.H, y = rit$SSC.H, xlab = "FSC (Height)", las = 1,
       ylab = "SSC (Height)", xlim = c(200, 220), ylim = c(170, 190),
       main = "(d) Lattice Structure of FCM Data")
  abline(h = seq(from = 170, by = 1, to = 190), col = "gray", lty = 1)
  abline(v = seq(from = 200, by = 1, to = 220), col = "gray", lty = 1)
  
  # Plot 5: Multi-Resolution Analysis Probability Map
  hier.rit <- gating(rit[, c("FSC.H", "SSC.H")], temp = 4, min.res = 64)
  plot.mrf_gating(hier.rit, main = "(e) Chapter 5: Probability Map",
  				  xlab = "FSC (Height)", ylab = "SSC (Height)", cex = 0)

  # Plot 6: Multi-Resolution Analysis Clusters
  plot(hier.rit$orig, xlim = c(0, 1023), ylim = c(0,1023),
       main = "(f) Chapter 5: Clusters", las = 1,
       xlab = "FSC (Height)", ylab = "SSC (Height)",
       col = ifelse(hier.rit$groups == 0, "grey", "blue"), cex = 0.2)
  