#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# last update: "2025-01-23"
# produced under R version: 2024.09.0
#---

# function for plotting parent posterior distributions

plot_posterior_dist <- function(combined_mcmcfin, parameter1, parameter2, col_param1, col_param2, bins, x_title){
  
  ggplot(combined_mcmcfin, aes(parameter1)) +
    geom_histogram(color = scales::alpha("black", 0.5), 
                   fill = col_param1, 
                   bins = bins) +
    geom_histogram(aes(parameter2), 
                   data = combined_mcmcfin,
                   color = scales::alpha("black", 0.5), 
                   fill = scales::alpha(col_param2, 0.4), 
                   bins = bins) +
    labs(x = x_title, y = "Posterior Density", title = '') + 
    coord_cartesian(ylim = c(0, 5000)) +
    theme_classic() +
    theme(axis.text.x=element_text(size=12),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          plot.margin = margin(t = 10,
                               r = 10,
                               b = 10,
                               l = 30),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 12),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size = 16))
  
}
