#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# last update: "2025-01-23"
# produced under R version: 2024.09.0
#---

# function for plotting posterior predictives

plot_posterior_predictives <- function(session_value, title){
  
  cols <- c("95% BCI" = alpha(color_error, 0.3),
            "alternative choice" = alpha(color_choice[1], 0.6),
            "eco choice" = alpha(color_choice[2], 0.6))
  
  # Plot data
  ggplot(frequency, aes(x = mid_bins)) +
    # histogram for choice == 1 - ecological choice (above the x-axis)
    geom_bar(data = subset(frequency, choice == 1 & session == session_value),
             stat = "identity",
             color = "black",
             size = 1.2,
             aes(y = count_emp, fill = "eco choice")) +
    
    # Shaded area for error bounds for choice == 1
    geom_ribbon(data = subset(frequency, choice == 1 & session == session_value),
                aes(ymin = lower_CI, ymax = upper_CI, fill = "95% BCI")) +
    
    # histogram for choice == 0 - non-ecological choice (above the x-axis)
    geom_bar(data = subset(frequency, choice == 0 & session == session_value),
             stat = "identity",
             color = "black",
             size = 1.2,
             #fill = color_choice[1],
             #alpha = 0.6,
             aes(y = -count_emp, fill = "alternative choice")) +
    
    # Shaded area for error bounds for choice == 1
    geom_ribbon(data = subset(frequency, choice == 0 & session == session_value),
                aes(ymin = -lower_CI, ymax = -upper_CI, fill = "95% BCI")) +
    
    # Adapt color
    scale_fill_manual(name = "", values = cols, guide = guide_legend(reverse = TRUE)) +
    
    # flip the y-axis
    scale_y_continuous(labels = abs) +
    labs(
      title = "",
      x = "Response Time (in sec)",
      y = "Frequency",
    ) + 
    coord_cartesian(ylim = c(-130, 190)) +
    ggtitle(title) +
    theme_bw() +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5,
                                    margin = margin(t = 10, r = 0, b = 20, l = 0)),
          axis.text.x = element_text(size = 12),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 12),
          plot.margin = margin(t = 5,
                               r = 5,
                               b = 10,
                               l = 5),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size = 16),
          axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 10), size = 14),
          legend.text = element_text(size = 16)) 
  
}
