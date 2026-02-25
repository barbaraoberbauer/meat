#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# last update: "2025-01-23"
# produced under R version: 2024.09.0
#---

# function for plotting change parameters as forest plot

plot_change_param_forest <- function(data, param, xtitle, cols){
  
  data %>%
    filter(parameter == param & session == "change") %>%
    ggplot(aes(x = mode, y = session, color = session)) +
    geom_vline(xintercept = 0, color = "#CB181D", linetype = "dashed", linewidth = 2) +
    geom_errorbar(aes(xmin=lowerHDI, xmax=upperHDI), width = 0, linewidth = 2.5, show.legend = FALSE) +
    geom_point(aes(fill = session), size = 8, shape = 22, color = "white", show.legend = FALSE) +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    scale_x_continuous(expand = expansion(mult = c(0.2, 0.2))) +
    labs(x = xtitle, title = "") + 
    theme_bw() +
    theme(#panel.grid.major = element_blank(),   
          #panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.title.y = element_blank(),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size = 14),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.margin = margin(t = 10,
                               r = 10,
                               b = 10,
                               l = 10)
    ) 
  
}