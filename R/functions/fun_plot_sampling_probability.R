
plot_sampling_probability <- function(data,
                                      minRequiredFixations,
                                      fix_var,
                                      attended_var,
                                      colValues,
                                      colLabels){
  
  fix_var <- enquo(fix_var)
  attended_var <- enquo(attended_var)
  
  plot <- data %>%
    filter(n >= minRequiredFixations) %>%
    ggplot(aes(x = !!fix_var,
               y = mean_prob,
               color = !!attended_var,
               group = !!attended_var,
               fill = !!attended_var)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
                alpha = 0.3) +
    geom_point() +
    geom_line() +
    scale_color_manual(values = colValues, 
                       labels = colLabels,
                       name = "") +
    scale_fill_manual(values = colValues, 
                      labels = colLabels,
                      name = "") +
    facet_grid(consumption_translation ~ session,
               labeller = labeller(
                 consumption_translation = labelsReplication,
                 session = labelsSession
               )) +
    coord_cartesian(ylim = c(0.1, 0.9)) +
    labs(x = "Fixation",
         y = "Sampling Probability") +
    theme(
      panel.border = element_rect(color = "black", 
                                  fill = NA, 
                                  linewidth = 1),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 12)
    )
  
  return(plot)
  
}

