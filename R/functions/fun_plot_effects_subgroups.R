plot_effects_subgroups <- function(attribute, x_title) {
  
  hdi_attr   <- hdi[[attribute]]
  modes_attr <- modes[[attribute]]
  
  ggplot() +
    # zero-line
    geom_segment(aes(x = 0, 
                     xend = 0, 
                     y = 0, 
                     yend = 4),
                 linewidth = 2,
                 linetype = "dashed",
                 color = color_error) +
    # absent
    geom_segment(aes(x = hdi_attr$hdi_absent_change[1], 
                     xend = hdi_attr$hdi_absent_change[2], 
                     y = 3, yend = 3),
                 linewidth = 3, color = "black") + 
    geom_point(aes(x = modes_attr$mode_absent_change,
                   y = 3),
               size = 7,
               shape = 15) +
    # present
    geom_segment(aes(x = hdi_attr$hdi_present_change[1], 
                     xend = hdi_attr$hdi_present_change[2], 
                     y = 2, yend = 2),
                 linewidth = 3, color = "black") +
    geom_point(aes(x = modes_attr$mode_present_change,
                   y = 2),
               size = 7,
               shape = 15) +
    # diff
    geom_segment(aes(x = hdi_attr$hdi_diff[1], 
                     xend = hdi_attr$hdi_diff[2], 
                     y = 1, yend = 1),
                 linewidth = 3, color = "darkgrey") +
    geom_point(aes(x = modes_attr$mode_diff,
                   y = 1),
               size = 7,
               shape = 15,
               color = "darkgrey") +
    coord_cartesian(ylim = c(0, 4)) +
    scale_y_continuous(
      breaks = 1:3,
      labels = c("PT present - \nPT absent", "PT present", "PT absent")
    ) +
    labs(x = x_title) +
    theme(
      axis.title.y = element_blank(),
      axis.text = element_text(size = 12),
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor = element_blank()
    )
}