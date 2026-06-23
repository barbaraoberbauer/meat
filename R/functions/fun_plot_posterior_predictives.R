#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# last update: "2025-01-23"
# produced under R version: 2024.09.0
#---

# function for plotting posterior predictives

plot_posterior_predictives <- function(frequency, session_value, maxRT, title){
  
  cols <- c("95% BCI" = scales::alpha(color_error, 0.3),
            "alternative choice" = scales::alpha(color_choice[1], 0.6),
            "env. choice" = scales::alpha(color_choice[2], 0.6))
  
    # Plot data
    ggplot(frequency, aes(x = mid_bins)) +
      # histogram for choice == 1 - ecological choice (above the x-axis)
      geom_bar(data = subset(frequency, choice == 1 & session == session_value),
               stat = "identity",
               color = "black",
               linewidth = 1.2,
               aes(y = count_emp, fill = "env. choice")) +
      # histogram for choice == 0 - non-ecological choice (above the x-axis)
      geom_bar(data = subset(frequency, choice == 0 & session == session_value),
               stat = "identity",
               color = "black",
               linewidth = 1.2,
               #fill = color_choice[1],
               #alpha = 0.6,
               aes(y = -count_emp, fill = "alternative choice")) +
      
      # Shaded area for error bounds for choice == 1
      geom_ribbon(data = subset(frequency, choice == 1 & session == session_value),
                  aes(ymin = lower_CI, ymax = upper_CI, fill = "95% BCI")) +
      
      # Shaded area for error bounds for choice == 1
      geom_ribbon(data = subset(frequency, choice == 0 & session == session_value),
                  aes(ymin = -lower_CI, ymax = -upper_CI, fill = "95% BCI")) +
    
    # Adapt color
    scale_fill_manual(name = "", values = cols, guide = guide_legend(reverse = TRUE)) +
    
    # flip the y-axis
    scale_y_continuous(labels = abs) +
    labs(
      title = title,
      x = "Response Time (in sec)",
      y = "Frequency",
    ) + 
    coord_cartesian(ylim = c(-150, 200), xlim = c(0, maxRT+1)) 
  
}
