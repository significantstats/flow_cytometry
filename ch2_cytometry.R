####------------------------------ Cytometry -------------------------------####

  # Load all files in functions directory
  sapply(list.files(path = 'functions', full.names = TRUE), source)

  # Load the Rituximab and GvHD Control Data
  rit <- read.csv(file = 'data/rituximab/rituximab.csv')
  gvhd.con <- read.csv(file = 'data/GvHD/GvHD_control.csv')
  
####---- 1. Rituximab plot -------------------------------------------------####
  
  # Set up the layout matrix for the graphics
  par(pty = "s")
  nf <- layout(matrix(c(1, 2, 3), nrow = 1, ncol = 3))

  # Plot 1: Stage 1 Gating
  lo.rit <- flowClust(x = rit, varNames = c("FSC.H", "SSC.H"), K = 1)
  plot(lo.rit, data = rit, las = 1, xlab = "FSC (Height)", 
       ylab = "SSC (Height)", main = "(a) Stage 1 Gate",
       xlim = c(0, 1023), ylim = c(0, 1023))

  # Plot 2: Stage 2 Gating - 2 clusters
  rit.gate <- rit[rit %in% lo.rit,]
  lo.rit.s2.c2 <- flowClust(rit.gate, varNames = c("FL3.H", "FL1.H"), K = 2)
  plot(lo.rit.s2.c2, data = rit.gate, las = 1, xlab = "7 AAD", 
       ylab = "Anti-BrdU FITC", main = "(b) Stage 2 Gates",
       xlim = c(0, 1023), ylim = c(0, 1023))
  mtext(text = "(2 gates)", cex = 0.7, line = 0.5)
  
  # Plot 3: Stage 2 Gating - 3 clusters
  lo.rit.s2.c3 <- flowClust(rit.gate, varNames = c("FL3.H", "FL1.H"), K = 3)
  plot(lo.rit.s2.c3, data = rit.gate, las = 1, xlab = "7 AAD", 
       ylab = "Anti-BrdU FITC", main = "(c) Stage 2 Gates",
       xlim = c(0, 1023), ylim = c(0, 1023))
  mtext(text = "(3 gates)", cex = 0.7, line = 0.5)
  
####---- 2. GvHD Control plot ----------------------------------------------#### 
  
  # Set up the layout matrix for the graphics
  par(pty = "s")
  nf <- layout(matrix(c(1, 2, 3), nrow = 1, ncol = 3))
  
  # Plot 1: Stage 2 Gating - 4 clusters
  gvhd.c4 <- flowClust(x = gvhd.con, varNames = c("CD3", "CD4"), K = 4)
  plot(gvhd.c4, data = gvhd.con, las = 1, xlab = "CD4 FITC", 
       ylab = "CD3 PerCP", main = "(a) Stage 2 Gates",
       xlim = c(0, 1023), ylim = c(0, 1023))
  mtext(text = "(4 gates)", cex = 0.7, line = 0.5)
  
  # Plot 2: Stage 2 Gating - 5 clusters
  gvhd.c5 <- flowClust(x = gvhd.con, varNames = c("CD3", "CD4"), K = 5)
  plot(gvhd.c5, data = gvhd.con, las = 1, xlab = "CD4 FITC", 
       ylab = "CD3 PerCP", main = "(b) Stage 2 Gates",
       xlim = c(0, 1023), ylim = c(0, 1023))
  mtext(text = "(5 gates)", cex = 0.7, line = 0.5)
  
  # Plot 3: Stage 2 Gating - 6 clusters
  gvhd.c6 <- flowClust(x = gvhd.con, varNames = c("CD3", "CD4"), K = 6)
  plot(gvhd.c6, data = gvhd.con, las = 1, xlab = "CD4 FITC", 
       ylab = "CD3 PerCP", main = "(c) Stage 2 Gates",
       xlim = c(0, 1023), ylim = c(0, 1023))
  mtext(text = "(6 gates)", cex = 0.7, line = 0.5)
  dev.off()
  