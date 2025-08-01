#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# last update: "2025-01-23"
# produced under R version: 2024.09.0
#---

# function for plotting change parameters

plot_change_param <- function(combined_mcmcfin, parameter, bins, x_title){
  
  ggplot(combined_mcmcfin, aes(parameter)) +
    geom_histogram(color = scales::alpha("black", 1), 
                   size = 1.2,
                   fill = "darkgrey", 
                   bins = bins) +
    geom_vline(xintercept = 0, color = "#CB181D", linetype = "dashed", linewidth = 1.2) +
    geom_vline(xintercept = quantile(parameter, c(.025,.975)), color = "black", linetype = "dashed", linewidth = 1.2) +
    labs(x = x_title, y = "", title = '') + 
    #coord_cartesian(ylim = c(0, 15000)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),   
          panel.grid.minor = element_blank(),
          axis.text.x=element_text(size=13),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          plot.margin = margin(t = 10,
                               r = 10,
                               b = 10,
                               l = 10),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size = 16),
    )
}
