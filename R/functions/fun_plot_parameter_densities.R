#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# produced under R version: 2024.09.0
#---

# function for plotting densities of parameter estimates for session 1 and 2

plot_group_means_densities <- function(data, param1, param2, paramName){
  
  ggplot(data = data) +
    geom_histogram(aes(x = param1, fill = "1"),
                   color = "black", linewidth = 1) +
    geom_histogram(aes(x = param2, fill = "2"),
                   color = "black", linewidth = 1) +
    scale_fill_manual(values = c("1" = scales::alpha(color_sessions[1], .8),
                                 "2" = scales::alpha(color_sessions[2], .8))) +
    labs(x = paramName, fill = "Session") + 
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.border = element_rect(color = "black", fill = NA))
  
}