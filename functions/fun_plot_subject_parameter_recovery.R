#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# produced under R version: 2024.09.0
#---

# function for plotting recovered subject parameters against true subject parameters

plot_subject_parameter_recovery <- function(data, parameter, xtitle, ytitle){
  
  cols <- c("black", "#E29501")
  
  data %>%
    filter(parameter == parameter) %>%
    ggplot(aes(x = generating_value)) +
    geom_point(aes(y = means), color = cols[1]) +
    geom_point(aes(y = medians), color = alpha(cols[2], 0.5)) +
    geom_abline(slope = 1,
                intercept = 0,
                color = "darkgrey",
                linetype = "dashed",
                linewidth = 2) +
    labs(x = xtitle,
         y = ytitle) +
    theme_bw() +
    theme(axis.text.x=element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0), size = 12))
  
}