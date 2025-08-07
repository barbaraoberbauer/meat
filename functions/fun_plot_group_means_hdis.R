#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# produced under R version: 2024.09.0
#---

# function for plotting group means and hdis

plot_group_means_hdis <- function(data, parameter, ytitle){
  
  data  %>%
    filter(parameters == parameter) %>%
    ggplot(aes(x = sim, y = generating_value)) +
    geom_errorbar(aes(ymin = hdi_lower, ymax = hdi_upper), 
                  width = 0, 
                  linewidth = 1.8,
                  color = "darkgrey") +
    geom_point(size = 5,
               shape = 15) +
    scale_x_continuous(breaks=seq(1, 10, 1)) +
    labs(x = "n simulation", 
         y = ytitle) + 
    theme_bw() +
    theme(axis.text.x=element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0), size = 12),
          axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0), size = 12)
    )
}