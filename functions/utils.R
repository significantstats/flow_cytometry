####-------------------------- Utility Functions ---------------------------####

####---- 1. Plot an image file using ggplot --------------------------------####

image_ggplot <- function(image) {
  
  ggplot(melt(image), aes(x = X1, y = X2, fill = factor(value))) + 
    labs(x = "", y = "", fill = "Value") + 
    geom_raster() +
    scale_fill_manual(values = c("white", "black")) +
    theme(plot.background = element_rect(fill = "black"), 
          legend.background = element_rect(fill = "black"),
          plot.margin = unit(c(0, 0, 0, 0), "null"),
          panel.spacing = unit(c(0, 0, 0, 0), "null"),
          panel.grid = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(),
          axis.ticks.length = unit(0, "null")) +
    guides(fill = FALSE) +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0))
}

####---- 2. Table for comparison of methods --------------------------------####

method_comparison <- function(A, B, proportion = FALSE) {
  
  comparison <- table(A, B)
  
  if (isTRUE(proportion)) {
    comparison <- comparison / sum(comparison)
  }
  
  return(comparison)
}