#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# last update: "2025-01-23"
# produced under R version: 2024.09.0
#---

# function for plotting parent posterior distributions as forest plots

plot_group_differences_forest <- function(data, param, group1, group2, difference, valuesLinewidth, valuesSize, groupLabels, xtitle, plotTitle, cols){

data %>%
  filter(parameter == param & group %in% c(group1, group2, difference)) %>%
  ggplot(aes(x = mode, y = group, color = group)) +
  geom_vline(xintercept = 0, color = "#CB181D", linetype = "dashed", linewidth = 2) +
  geom_errorbar(aes(xmin=lowerHDI, xmax=upperHDI, linewidth = group), width = 0, show.legend = FALSE) +
  scale_linewidth_manual(values = valuesLinewidth) +
  geom_point(aes(fill = group, size = group), shape = 22, color = "white", show.legend = FALSE) +
  scale_size_manual(values = valuesSize) +
  scale_y_discrete(limits = c(difference, group1, group2),
                   labels = groupLabels) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  labs(x = xtitle, title = plotTitle, color = "Group", fill = "Group") + 
  theme_bw() +
  theme(plot.title = element_text(size = 16, margin = margin(t = 0, r = 0, b = 10, l = 0), hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.y = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size = 14),
        plot.margin = margin(t = 10,
                             r = 10,
                             b = 10,
                             l = 10)
  ) 

}
