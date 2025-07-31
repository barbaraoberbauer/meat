
plot_group_differences <- function(data, group1, group2, diff, bins, x_title, label_group1, label_group2, label_diff, annotation_y, ylim_upper){
  
  # set color
  col_effect <- "#6A66A3"
  
  # # get density at mean(diff) to set label correctly
  # hist_diff <- hist(diff, breaks = 5, plot = FALSE)
  # bin_index <- findInterval(mean(diff), hist_diff$breaks, rightmost.closed = TRUE)
  # density_in_bin <- hist_diff$density[bin_index]
  # annotation_y <- 3000 + density_in_bin * length(diff) * diff(range(diff))/bins
  
  
  ggplot(data, aes(group1)) +
    # Add group effects
    geom_histogram(color = "black",
                   size = 0.8,
                   fill = alpha("lightgrey",0.4),
                   bins = bins) +
    geom_histogram(aes(group2),
                   color = "black",
                   size = 0.8,
                   fill = alpha("lightgrey",0.4),
                   bins = bins) +
    
    geom_histogram(aes(diff),
                   color = "black",
                   size = 1.2,
                   fill = alpha(col_effect, 0.7),
                   bins = bins) +
    geom_vline(xintercept = 0, color = "#CB181D", linetype = "dashed", linewidth = 1.2) +
    geom_vline(xintercept = quantile(diff, c(.025,.975)), color = "black", linetype = "dashed", linewidth = 1.2) +
    coord_cartesian(ylim = c(-2000, ylim_upper)) +
    # Add labels
    annotate("text", 
             x = mean(group1), 
             y = -1200, 
             label = label_group1, 
             color = "darkgrey", 
             size = 4.5) + # text for control group
    annotate("text", 
             x = mean(group2), 
             y = -1200, 
             label = label_group2, 
             color = "darkgrey", 
             size = 4.5) + # text for rating group
    annotate("label", 
             x = mean(diff), 
             y = annotation_y, 
             label = label_diff, 
             color = col_effect,
             fill = "white",
             size = 5,
             fontface = 2) + # text for effect
    labs(x = x_title, y = "Posterior Density", title = '') +
    theme_bw() +
    theme(panel.grid.major = element_blank(),   
          panel.grid.minor = element_blank(),
          axis.text.x=element_text(size=12),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          plot.margin = margin(t = 10,
                               r = 10,
                               b = 10,
                               l = 10),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 12),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size = 14))
  
}


