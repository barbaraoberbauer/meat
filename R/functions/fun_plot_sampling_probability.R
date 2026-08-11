
plot_sampling_probability <- function(data,
                                      minRequiredFixations,
                                      fix_var,
                                      attended_var,
                                      colValues,
                                      colLabels,
                                      ylim){
  
  fix_var <- enquo(fix_var)
  fix_var_name <- as_name(fix_var)
  attended_var <- enquo(attended_var)
  
  plot_data <- data %>%
    filter(n >= minRequiredFixations) 
    
  # Determine marker position + label depending on direction of fix_var
  if (fix_var_name == "fixNum") {
    marker_x     <- 1
    marker_label <- "S"
  } else if (fix_var_name == "fixNumRev") {
    marker_x     <- -1
    marker_label <- "R"
  } 
  
  plot <- plot_data %>%
    ggplot(aes(x = !!fix_var,
               y = mean_prob,
               color = !!attended_var,
               group = !!attended_var,
               fill = !!attended_var)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
                alpha = 0.3) +
    geom_point() +
    geom_line() +
    geom_vline(xintercept = marker_x,
               linetype = "dashed",
               color = "grey30",
               linewidth = 0.5) +
    scale_x_continuous(
      breaks = ~ union(seq(round(.x[1]), round(.x[2]), by = 5), marker_x),
      labels = function(x) ifelse(x == marker_x, 
                                  paste0("**", marker_label, "**"), 
                                  as.character(x))
    ) +
    scale_color_manual(values = colValues, 
                       labels = colLabels,
                       name = "") +
    scale_fill_manual(values = colValues, 
                      labels = colLabels,
                      name = "") +
    facet_grid(session ~ consumption_translation,
               labeller = labeller(
                 consumption_translation = labelsReplication,
                 session = labelsSession
               )) +
    coord_cartesian(ylim = ylim) +
    labs(x = "Fixation",
         y = "Sampling Probability") +
    theme(
      panel.border = element_rect(color = "black", 
                                  fill = NA, 
                                  linewidth = 1),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 12),
      axis.text.x = element_markdown()
    )
  
  return(plot)
  
}

