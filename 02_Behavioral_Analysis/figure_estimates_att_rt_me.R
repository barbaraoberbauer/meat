#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# last update: "2025-02-17"
# produced under R version: 2024.09.0
#---

# Load packages and read data ------

### Clear environment -------

#clear working environment
rm(list=ls())

#clear all plots
if(!is.null(dev.list())) dev.off()

### Install packages -------

# List of packages to check and install if necessary
packages <- c("tidyverse",
              "dplyr",
              "ggplot2",
              "cowplot")

# Function to check if a package is installed
is_package_installed <- function(package_name) {
  is.element(package_name, installed.packages()[, "Package"])
}

# Iterate through the list of packages
for (package in packages) {
  if (!is_package_installed(package)) {
    # Install the package
    install.packages(package)
  }
}

# Load required libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(cowplot)


rm(package, packages, is_package_installed)


### Load data ---------

results_att_rt <- readRDS("data/results_att_rt.rds")

# red = reduced model (main effects of condition)
results_att_red <- as.data.frame(results_att_rt$results_att_red)
results_rt_red <- as.data.frame(results_att_rt$results_rt_red)


# Check data structure -------

str(results_att_red)
str(results_rt_red)

### reverse factor levels for plot ----

results_att_red$consumption_translation <- factor(x = results_att_red$consumption_translation,
                                              levels = rev(levels(results_att_red$consumption_translation)))

results_rt_red$consumption_translation <- factor(x = results_rt_red$consumption_translation,
                                             levels = rev(levels(results_rt_red$consumption_translation)))


# Plot model estimates -----

### Attention ----

p_att <- 
ggplot(data = results_att_red,
       mapping = aes(x = estimate, y = consumption_translation)) +
  geom_vline(xintercept = 0, linetype = 'dashed', size = 1.5, color = "darkgrey") +
  geom_point(size = 3.5) +
  geom_errorbar(aes(xmin=asymp.LCL, xmax=asymp.UCL), width = .2, size = 1.5) +
  labs(
    x = "Effect on Relative Difference in Dwell Time",
    y = "Consumption Translation"
  ) +
  coord_cartesian(xlim = c(-0.05, 0.1)) +
  scale_y_discrete(labels = c("control" = "Control",
                              "operating_costs" = "Operating\nCosts",
                              "emissions" = "Carbon\nEmissions",
                              "environmental_friendliness" = "Rating")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 14,
                                    margin = margin(t = 15, r = 0, b = 0 ,l = 0)),
        axis.title.y = element_text(size = 14,
                                    margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.margin = margin(t = 15,
                             r = 15,
                             b = 15,
                             l = 15)
  )



### RT ----

p_rt <- 
  ggplot(data = results_rt_red,
         mapping = aes(x = estimate, y = consumption_translation)) +
  geom_vline(xintercept = 0, linetype = 'dashed', size = 1.5, color = "darkgrey") +
  geom_point(size = 3.5) +
  geom_errorbar(aes(xmin=asymp.LCL, xmax=asymp.UCL), width = .2, size = 1.5) +
  labs(
    x = "Effect on Response Times (in sec)",
    y = "Consumption Translation"
  ) +
  coord_cartesian(xlim = c(-2.5, 5)) +
  scale_y_discrete(labels = c("control" = "Control",
                              "operating_costs" = "Operating\nCosts",
                              "emissions" = "Carbon\nEmissions",
                              "environmental_friendliness" = "Rating")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 14,
                                    margin = margin(t = 15, r = 0, b = 0 ,l = 0)),
        axis.title.y = element_text(size = 14,
                                    margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.margin = margin(t = 15,
                             r = 15,
                             b = 15,
                             l = 15)
  )


# Save plots
saveRDS(p_att, "figures/p_att.rds")
saveRDS(p_rt, "figures/p_rt.rds")

  
# ### Arrange grids -----
# 
# plot_att_rt <- plot_grid(p_att,
#                          p_rt,
#                          ncol = 1,
#                          labels = c("a", "b"),
#                          label_size = 20
# )
# 
# # save plot
# ggsave("figures/plot_att_rt_me.png", plot_att_rt, width = 6, height = 9)



