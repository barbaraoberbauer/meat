#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# produced under R version: 2024.09.0
#---

# function for plotting recovered subject parameters against true subject parameters

plot_subject_parameter_recovery <- function(data, param, xtitle, ytitle, correlation){
  
  #cols <- c("black", "#E29501")
  
  # cor_mean <- correlation %>%
  #   filter(parameter == param) %>%
  #   pull(cor_mean)

  cor_median <- correlation %>%
    filter(parameter == param) %>%
    pull(cor_median)
  
  cor_label <- paste0(
    #"Cor mean = ", signif(cor_mean, 3), "\n",
    "Cor median = ", signif(cor_median, 3)
  )
  
  data %>%
    filter(parameter == param) %>%
    ggplot(aes(x = generating_value)) +
    #geom_point(aes(y = means), color = cols[1]) +
    geom_point(aes(y = medians), color = alpha("darkgrey", 0.5)) +
    geom_abline(slope = 1,
                intercept = 0,
                color = "black",
                linetype = "dashed",
                linewidth = 1.5) +
    labs(x = xtitle,
         y = ytitle) +
    theme_bw() +
    theme(axis.text.x=element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0), size = 12)) +
    # Add the correlation in the upper right corner 
    annotate("text",
             x = Inf,
             y = -Inf,
             label = cor_label,
             hjust = 1.1,  # Adjust as needed for alignment
             vjust = -0.5,
             size = 4)
  
}