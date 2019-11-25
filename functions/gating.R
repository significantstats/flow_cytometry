gating <- function(fcm, temp = 4, min = 0, max = 1023, min.res = 128) {
  
  # Put the two variables of interest in an NxN matrix (field)  
  mat.grid <- fcm_to_image(fcm, min = min, max = max)
  dimension <- nrow(mat.grid)
  
  # Remove all extremity values (keep record of their positions)
  mat.grid[ , dimension] <- mat.grid[dimension, ] <- 0 
  extreme.values <- unique(c(which(fcm[,1] == max), which(fcm[,2] == max)))
  removals <- rep(0, times = nrow(fcm))
  removals[extreme.values] <- 1
  
  # Multi-Level application of the Hierarchical Approach
  ## First Grid
  mat.grid.red <- grid_red(x = mat.grid, red.dim = min.res, dimension = dimension)
  hier.grid <- hierarchical(state = mat.grid.red[[1]], temp = temp)
  
  ## Second Grid
  hier.grid$state <- grid_inc(spatial_smooth(hier.grid$state), nrow(hier.grid$state))
  hier.grid <- hierarchical(state = hier.grid$state, temp = temp)

  # Third Grid
  hier.grid$state <- grid_inc(spatial_smooth(hier.grid$state), nrow(hier.grid$state))
  hier.grid <- hierarchical(state = hier.grid$state, temp = temp)
  
  # Probability Map
  neigh.ones <- neigh_system(spatial_smooth(hier.grid$state), value = 1, 
                             dimension = nrow(hier.grid$state))
  neigh.zeros <- neigh_system(spatial_smooth(hier.grid$state), value = 0, 
                              dimension = nrow(hier.grid$state))
  grid.probs <- exp(neigh.ones) / (exp(neigh.zeros) + exp(neigh.ones))
  
  # Connected Components Algorithm
  groups.grid <- SDMTools::ConnCompLabel(spatial_smooth(hier.grid$state))
  
  # Scale up the grid to the correct size
  while (nrow(groups.grid) != dimension) {
  	groups.grid <- grid_inc(groups.grid, nrow(groups.grid))
  }
  
  # Identify the groups and return to data frame format
  groups <- image_to_fcm(x = groups.grid, original = fcm, min = min, max = max)
  groups$groups[which(removals == 1)] <- NA
  
  # Output to return to the user
  output <- structure(list(orig = fcm, groups = groups$groups, 
                           removals = removals, grid.probs = grid.probs))
  
  return(output)
}


plot.mrf_gating <- function(object, xlab = NULL, ylab = NULL, main = NULL, cex = 0.1) {
  
  if (is.null(xlab)) {
    xlab <- names(object$orig)[[1]]
  }
  
  if (is.null(ylab)) {
    ylab <- names(object$orig)[[2]]
  }
  
  if (is.null(main)) {
    main <- ""
  }
  
  no.row <- nrow(object$grid.probs)
  no.col <- ncol(object$grid.probs)
  axis.pos <- c(0, 200, 400, 600, 800, 1000) * ((no.row - 1) / 1023)
  
  # Initilise Plot
  par(pty = "s")
  plot(0, type = "n", las = 1, xlab = xlab, ylab = ylab,
       main = main, xlim = c(-0.5, no.row), ylim = c(-0.5, no.col),
       xaxt = "n", yaxt = "n", bty = "o")
  axis(side = 1, at = axis.pos, labels = c(0, 200, 400, 600, 800, 1000))
  axis(side = 2, at = axis.pos, labels = c(0, 200, 400, 600, 800, 1000), las = 1)
  
  # Add probability map layer
  colours.scale <- c("yellow", "orange", "red")
  
  probs.x <- rep(1:no.row, times = no.col)
  probs.y <- sort(rep(1:no.col, times = no.row))
  probs.val <- as.vector(object$grid.probs)
  probs.col <- ifelse(probs.val <= 0.3, colours.scale[1],
                      ifelse(probs.val <= 0.90, colours.scale[2], colours.scale[3]))
  probs.plot <- ifelse(probs.val < 0.1, FALSE, TRUE)
  
  for (i in 1:length(probs.x)) {
    if (probs.plot[i] == TRUE) {
      polygon(x = c((probs.x[i] - 1.5), (probs.x[i] - 1.5), 
                    (probs.x[i] - 0.5), (probs.x[i] - 0.5)), 
              y = c((probs.y[i] - 1.5), (probs.y[i] - 0.5), 
                    (probs.y[i] - 0.5), (probs.y[i] - 1.5)),
              col = probs.col[i], border = NA)
    }
  }
  
  # Add points coloured by cluster
  points.characters <- ifelse(is.na(object$groups), 8, 1)
  points.size <- ifelse(is.na(object$groups), 0.2, 
                        ifelse(object$groups == 0, 0.2, cex))
  points.colours <- ifelse(is.na(object$groups), "grey",
                           ifelse(object$groups == 0, "grey", "black"))
  points(object$orig * ((no.row - 1) / 1023), col = points.colours, pch = points.characters, cex = points.size)
  
  invisible()
}