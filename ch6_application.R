####-------------------- Application to Flow Cytometry ---------------------####

  # Load all files in functions directory
  sapply(list.files(path = 'functions', full.names = TRUE), source)

####---- 6.2 Application to Rituximab Data ---------------------------------####

  # Load the Rituximab Data
  rit <- read.csv(file = 'data/rituximab/rituximab.csv')

  # Flow Clust Model
  Lo.rit <- flowClust(rit, varNames = c("FSC.H", "SSC.H"), K = 1, 
                      z.cutoff = 0.5)
  Lo.rit.gate <- rit[rit %in% Lo.rit, c("FL3.H", "FL1.H")]
  Lo.rit.stagetwo <- flowClust(Lo.rit.gate, K = 3)

  # Hierarchical Model
  hier.rit <- gating(rit[, c("FSC.H", "SSC.H")], temp = 4, min.res = 64)
  hier.rit.gate <- rit[which(hier.rit$groups == 1), c("FL3.H", "FL1.H")]
  hier.rit.stagetwo <- gating(hier.rit.gate, temp = 4, min.res = 64)
  
  # Rituximab Plot
  par(mfrow = c(3,2))
  par(pty = "s")
  plot(Lo.rit, data = rituximab, xlab = "FSC (Height)", 
       ylab = "SSC (Height)", las = 1, 
       main = "(a) t-mixture with Box-Cox", show.outliers = TRUE,
       pch.outliers = 20, cex = 1.5, cex.outliers = 0.4)
  plot(Lo.rit.stagetwo, data = Lo.rit.gate, xlab = "7 AAD", 
       ylab = "Anti-BrdU FITC", las = 1, 
       xlim = c(0, 1023), ylim = c(0, 1023),
       main = "(d) t-mixture with Box-Cox", show.outliers = TRUE,
       pch.outliers = 20, cex = 1.5, cex.outliers = 0.4)     
  
  plot.mrf_gating(hier.rit, main = "(b) Hierarchical Probabilities",
  				  xlab = "FSC (Height)", ylab = "SSC (Height)", cex = 0.05)
  plot.mrf_gating(hier.rit.stagetwo, main = "(e) Hierarchical Probabilities",
    			  xlab = "7 AAD", ylab = "Anti-BrdU FITC", cex = 0.05)
  
  plot(hier.rit$orig, xlim = c(0, 1023), ylim = c(0,1023),
       main = "(c) Hierarchical clusters", las = 1,
       xlab = "FSC (Height)", ylab = "SSC (Height)",
       col = ifelse(hier.rit$groups == 0, "grey", "blue"), cex = 0.2)
  plot(hier.rit.stagetwo$orig, xlim = c(0, 1023), ylim = c(0,1023),
       main = "(f) Hierarchical Clusters", las = 1,
       xlab = "7 AAD", ylab = "Anti-BrdU FITC",
       col = ifelse(hier.rit.stagetwo$groups == 0, "grey", 
                    (hier.rit.stagetwo$groups + 2)), cex = 0.2)
  par(mfrow = c(1, 1))
  
####---- 6.3 Application to GvHD Data --------------------------------------####
  
  # Source GvHD Control and Positive data
  GvHD.con <- read.csv('data/GvHD/GvHD_control.csv')
  GvHD.pos <- read.csv('data/GvHD/GvHD_positive.csv')
  
  # Flow Clust Methods
  Lo.GvHDcon.CD8B <- flowClust(GvHD.con, varNames = c("CD4", "CD8b"), K = 4)
  Lo.GvHDcon.CD3 <- flowClust(GvHD.con, varNames = c("CD4", "CD3"), K = 5)
  Lo.GvHDpos.CD8B <- flowClust(GvHD.pos, varNames = c("CD4", "CD8b"), K = 6)
  Lo.GvHDpos.CD3 <- flowClust(GvHD.pos, varNames = c("CD4", "CD3"), K = 5)
  
  # Hierarchical Methods
  hier.GvHDcon.CD8B <- gating(GvHD.con[,c("CD4", "CD8b")], temp = 4)
  hier.GvHDcon.CD3 <- gating(GvHD.con[,c("CD4", "CD3")], temp = 4)
  hier.GvHDpos.CD8B <- gating(GvHD.pos[,c("CD4", "CD8b")], temp = 4)
  hier.GvHDpos.CD3 <- gating(GvHD.pos[,c("CD4", "CD3")], temp = 4)
  
  # GvHD Control Plot
  par(mfrow = c(3,2))
  par(pty = "s")
  plot(Lo.GvHDcon.CD8B, data = GvHD.con, las = 1, pch.outliers = 20, 
       ylab = expression(paste("CD8", beta, " PE")), xlab = "CD4 FITC", 
       xlim = c(0, 1023), ylim = c(0, 1023), cex = 1.5, cex.outliers = 0.4,
       main = "(a) t-mixture with Box-Cox", show.outliers = TRUE)
  plot(Lo.GvHDcon.CD3, data = GvHD.con, las = 1, pch.outliers = 20,   
       ylab = "CD3 PerCP", xlab = "CD4 FITC",
       xlim = c(0, 1023), ylim = c(0, 1023), cex = 1.5, cex.outliers = 0.4,
       main = "(d) t-mixture with Box-Cox", show.outliers = TRUE)
              
  plot.mrf_gating(hier.GvHDcon.CD8B, main = "(b) Hierarchical probabilities",
       xlab = "CD4 FITC", ylab = expression(paste("CD8", beta, " PE")), cex = 0.05)
  plot.mrf_gating(hier.GvHDcon.CD3, main = "(e) Hierarchical probabilities",
       ylab = "CD3 PerCP", xlab = "CD4 FITC", cex = 0.05)
  
  plot(hier.GvHDcon.CD8B$orig, xlim = c(0, 1023), ylim = c(0,1023),
       xlab = "CD4 FITC", ylab = expression(paste("CD8", beta, " PE")),
       main = "(c) Hierarchical clusters", las = 1,
       col = ifelse(hier.GvHDcon.CD8B$groups == 0, "grey", 
                    hier.GvHDcon.CD8B$groups), cex = 0.4)
  plot(hier.GvHDcon.CD3$orig, xlim = c(0, 1023), ylim = c(0,1023),
       main = "(f) Hierarchical clusters", las = 1,
       ylab = "CD3 PerCP", xlab = "CD4 FITC",
       col = ifelse(hier.GvHDcon.CD3$groups == 0, "grey", 
                    hier.GvHDcon.CD3$groups), cex = 0.4)
  par(mfrow = c(1, 1))
  
  # GvHD Positive Plot
  par(mfrow = c(3,2))
  par(pty = "s")
  plot(Lo.GvHDpos.CD8B, data = GvHD.pos, las = 1, pch.outliers = 20, 
       ylab = expression(paste("CD8", beta, " PE")), xlab = "CD4 FITC", 
       xlim = c(0, 1023), ylim = c(0, 1023),
       cex = 1.5, cex.outliers = 0.4,
       main = "(a) t-mixture with Box-Cox", show.outliers = TRUE)
  plot(Lo.GvHDpos.CD3, data = GvHD.pos, las = 1, pch.outliers = 20,
       ylab = "CD3 PerCP", xlab = "CD4 FITC", 
       xlim = c(0, 1023), ylim = c(0, 1023),
       cex = 1.5, cex.outliers = 0.4,
       main = "(d) t-mixture with Box-Cox", show.outliers = TRUE)
            
  plot.mrf_gating(hier.GvHDpos.CD8B, main = "(b) Hierarchical probabilities",
       xlab = "CD4 FITC", ylab = expression(paste("CD8", beta, " PE")), cex = 0.05)
  plot.mrf_gating(hier.GvHDpos.CD3, main = "(e) Hierarchical probabilities",
       ylab = "CD3 PerCP", xlab = "CD4 FITC", cex = 0.05)
  
  plot(hier.GvHDpos.CD8B$orig, xlim = c(0, 1023), ylim = c(0,1023),
       main = "(c) Hierarchical clusters", las = 1,
       xlab = "CD4 FITC", ylab = expression(paste("CD8", beta, " PE")),
       col = ifelse(hier.GvHDpos.CD8B$groups == 0, "grey", 
                    hier.GvHDpos.CD8B$groups), cex = 0.4)  
  plot(hier.GvHDpos.CD3$orig, xlim = c(0, 1023), ylim = c(0,1023),
       main = "(f) Hierarchical clusters", las = 1,
       ylab = "CD3 PerCP", xlab = "CD4 FITC",
       col = ifelse(hier.GvHDpos.CD3$groups == 0, "grey", 
                    hier.GvHDpos.CD3$groups), cex = 0.4)
  par(mfrow = c(1, 1))