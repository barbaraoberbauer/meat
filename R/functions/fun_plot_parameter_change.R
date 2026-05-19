#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# last update: "2025-01-23"
# produced under R version: 2024.09.0
#---

# function for plotting change parameters 

plot_change_param <- function(param, hdi_range, xtitle){
  
  h <- hist(param, plot = FALSE)
  hist_df <- data.frame(
    mids   = h$mids,
    counts = h$counts,
    width  = diff(h$breaks)[1]
  )
  
  ggplot(hist_df, aes(x = mids, y = counts)) +
    geom_col(fill = "white", color = "black", linewidth = 1, width = hist_df$width[1]) +
    geom_col(data = subset(hist_df, mids >= hdi_range[1] & mids <= hdi_range[2]),
             fill = "darkgrey", color = "black", linewidth = 1,
             width = hist_df$width[1]) +
    geom_vline(xintercept = 0, color = "#CB181D", linetype = "dashed", linewidth = 2.5) +
    labs(x = xtitle) +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.border = element_rect(color = "black", fill = NA))
  
}