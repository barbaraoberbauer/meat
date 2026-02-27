#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# last update: "2025-01-23"
# produced under R version: 2024.09.0
#---

# function for plotting model estimates for attentional and rt data

plot_model_estimates <- function(data, data_red, xtitle, xlim, labels){
  
  ggplot(data = data,
         mapping = aes(x = estimate, 
                       y = consumption_translation, 
                       color = price_translation)) +
    geom_vline(xintercept = 0, 
               linetype = 'dashed', 
               size = 1) +
    geom_point(position = position_nudgedodge(y=0.15, 
                                              width=0.2), 
               size = 2.5) +
    geom_errorbar(aes(xmin=asymp.LCL, 
                      xmax=asymp.UCL), 
                  width=.1, 
                  size = 1,
                  position = position_nudgedodge(y=0.15, 
                                                 width=0.2)) +
    
    labs(
      x = xtitle,
      y = "Consumption Translation",
      color = "Price Translation"
    ) +
    coord_cartesian(xlim = xlim) +
    scale_y_discrete(labels = labels) +
    scale_color_manual(values = color_price,
                       breaks = c("0", "1"),
                       labels = c("1" = "Present",
                                  "0" = "Absent")) +
    theme(panel.border = element_rect(color = "black", fill = NA),
          panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_line(color = "grey90")
          ) +
    
    # add main effect of consumption translation (data_red)
    geom_point(data = data_red, aes(x = estimate, y = consumption_translation), 
               position = position_nudgedodge(y=-0.15, width=0), 
               size = 3.5, color = "#C33C54") +
    geom_errorbar(data = data_red, aes(xmin=asymp.LCL, xmax=asymp.UCL, 
                                       color = NULL), width = .2, size = 1.5,
                  position = position_nudgedodge(y=-0.15, width=0), color = "#C33C54") 
  
}