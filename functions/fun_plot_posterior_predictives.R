#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# last update: "2025-01-23"
# produced under R version: 2024.09.0
#---

# function for plotting posterior predictives

plot_posterior_predictives <- function(session_value, title){
  
  ggplot(frequency, aes(x = mid_bins)) +
    # histogram for choice == 1 - ecological choice (above the x-axis)
    geom_bar(data = subset(frequency, choice == 1 & session == session_value),
             stat = "identity",
             color = "black",
             fill = color_choice[2],
             alpha = 0.6,
             aes(y = count_emp)) +
    
    # Shaded area for error bounds for choice == 1
    geom_ribbon(data = subset(frequency, choice == 1 & session == session_value),
                aes(ymin = lower_bci, ymax = upper_bci),
                fill = color_error,
                alpha = 0.3) + # Adjust transparency as needed
    
    # histogram for choice == 0 - non-ecological choice (above the x-axis)
    geom_bar(data = subset(frequency, choice == 0 & session == session_value),
             stat = "identity",
             color = "black",
             fill = color_choice[1],
             alpha = 0.6,
             aes(y = -count_emp)) +
    
    # Shaded area for error bounds for choice == 1
    geom_ribbon(data = subset(frequency, choice == 0 & session == session_value),
                aes(ymin = -lower_bci, ymax = -upper_bci),
                fill = color_error,
                alpha = 0.3) + # Adjust transparency as needed
    
    # flip the y-axis
    scale_y_continuous(labels = abs) +
    labs(
      title = "",
      x = "Response Time (in sec)",
      y = "Frequency"
    ) +
    coord_cartesian(ylim = c(-130, 130)) +
    ggtitle(title) +
    theme_classic() +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5,
                                    margin = margin(t = 10, r = 0, b = 20, l = 0)),
          axis.text.x = element_text(size = 12),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 12),
          plot.margin = margin(t = 10,
                               r = 10,
                               b = 10,
                               l = 10),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size = 12),
          axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 10), size = 12))
  
}
