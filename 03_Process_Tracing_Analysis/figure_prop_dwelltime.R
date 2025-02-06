#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# last update: "2025-01-15"
# produced under R version: 2024.09.0
#---

# Code for producing Figure XY 

# Load packages and read data ------------

#clear working environment
rm(list=ls())

#clear all plots
if(!is.null(dev.list())) dev.off()


### Install packages -------

# List of packages to check and install if necessary
packages <- c("tidyverse",
              "ggplot2",
              "cowplot",
              "gridExtra",
              "ggpubr")

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
library(gridExtra)
library(ggpubr)

rm(package, packages, is_package_installed)


### Load data ---------

df <- readRDS("data/df.rds")


### Load color paletts ----------

color_choice <- c("#CBCBD4", "#556F44") # non-eco, eco choice
color_session <- c("#6A66A3", "#C33C54") # session 1, session 2


### Extend ggplot2's GeomViolin ----------

# following: https://stackoverflow.com/questions/35717353/split-violin-plot-with-ggplot2 

GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin,
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ...,
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}


# Aggregate difference between option dwell times (diff_t_options) --------

dt_options <- df %>%
  group_by(id, session, consumption_translation) %>%
  summarize(diff_t = mean(diff_t_options))

### check structure
str(dt_options)

dt_options <- dt_options %>%
  mutate(consumption_translation = factor(consumption_translation, levels = c("control", 
                                                            "emissions", 
                                                            "operating_costs",
                                                            "environmental_friendliness")))


# Plot diff_t_options -------


dt_options_plot <- 
  dt_options %>%
  ggplot(mapping = aes(x = consumption_translation, y = diff_t, fill = session)) +
  
  # violin blot
  geom_split_violin(size = 0.6) +
  
  # box plot
  geom_boxplot(aes(fill = session, colour = session),
               width = 0.12, 
               position = position_dodge(0.3), 
               show.legend = FALSE) +
  
  # color
  scale_fill_manual(values = scales::alpha(color_session, 0.4),
                    labels = c("1" = "1",
                               "2" = "2")) +
  
  scale_color_manual(values = color_session,
                     labels = c("1" = "1", 
                                "2" = "2")) +
  
  # axes and titles
  coord_cartesian(ylim = c(-0.4, 0.4)) +
  labs(
    title = "Differential Dwell Time",
    x = "Condition",
    y = "Differential Dwell Time",
    fill = "Session"
  ) +
  scale_x_discrete(labels = c("control" = "Control",
                              "operating_costs" = "Operating Costs",
                              "emissions" = "Carbon Emissions",
                              "environmental_friendliness" = "Rating")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, 
                                  face = "bold", 
                                  hjust = 0.5,
                                  margin = margin(t = 0, r = 0, b = 10, l = 0)),
        axis.title.x = element_text(size = 12,
                                    margin = margin(t = 15, r = 0, b = 0 ,l = 0)),
        axis.title.y = element_text(size = 12,
                                    margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5, "cm"),
        legend.position = "top",
        plot.margin = margin(t = 15,
                             r = 15,
                             b = 15,
                             l = 15),
        panel.spacing = unit(0.7, "lines")
  )


# Save plot
ggsave("figures/figure_prop_dwellTime.png", dt_options_plot, width = 8, height = 5, bg = "white")




