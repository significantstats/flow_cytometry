plot_sims <- function(estimates, sims) {
  
  ggplot(data = estimates, aes(x = theta)) +
    # Add line and points for analytic solution for expected energy
    geom_line(aes(y = analytic_exp), 
              colour = "blue", size = 1, alpha = 0.5) +
    geom_point(aes(y = numeric_exp, colour = "blue"),
               size = 3, shape = 16) +
    # Add points for estimated expected energy
    geom_point(aes(y = numeric_exp, colour = "red"),
               size = 3, shape = 16) +
    # Add simulation points
    geom_count(data = sims, aes(x = p, y = E, size = ..prop.., group = p), 
               colour = "black", shape = 1) +
    scale_size_area(max_size = 6, 
                    guide = guide_legend(title = "Simulated Proportion", 
                                         title.position = "top")) +
    # Add line at zero
    geom_hline(yintercept = 0, colour = "lightgray", size = 1, lty = 2) +
    # Add legend correctly
    scale_colour_manual(name = "Expectation", values = c("blue", "red"),
                        labels = c("Analytic", "Simulations"),
                        guide = guide_legend(title.position = "top")) +
    # Add title, x-axis label and y-axis label
    labs(title = paste("Energy distribution on a", 
                       sims$N[1], "x", sims$M[1], "lattice"),
         x = "Theoretical Probability", y = "Energy") +
    # Black and White Theme
    theme_bw() +
    # Remove Gridlines from the plot
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = "bottom")
  
}