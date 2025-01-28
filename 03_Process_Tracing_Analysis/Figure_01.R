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
color_session <- c("#8cc5e3", "#2066a8") # session 1, session 2


# Calculate dwell time and choice probability ----

outcome <- df %>%
  group_by(id, condition, session) %>%
  summarize(dt_0 = mean(t_option0),
            dt_1 = mean(t_option1),
            cp_0 = sum(choice == 0)/n(),
            cp_1 = sum(choice == 1)/n())


### collapse subgroups across price attribute manipulation

outcome <- outcome %>%
  mutate(group = case_when(
    condition %in% c(1, 2) ~ "control",
    condition %in% c(3, 4) ~ "operating_costs",
    condition %in% c(5, 6) ~ "carbon_emissions",
    condition %in% c(7, 8) ~ "rating",
    TRUE ~ NA_character_  # handles any other cases
  ))

### remove condition
outcome <- subset(outcome, select = -condition)


### check structure of data frame

str(outcome)

outcome <- outcome %>%
  mutate(group = factor(group, levels = c("control", 
                                          "carbon_emissions", 
                                          "operating_costs",
                                          "rating")))

### create data frame for dwell time

dwell_time <- outcome %>%
  pivot_longer(cols = c("dt_0","dt_1"),
               names_to = "choice_option",
               names_prefix = "dt_",
               values_to = "dwell_time")

### remove choice probability
dwell_time <- subset(dwell_time, select = -c(cp_0, cp_1))

### transform ms to s
dwell_time$dwell_time <- dwell_time$dwell_time/1000


### create data frame for choice probability

choice_prob <- outcome %>%
  pivot_longer(cols = c("cp_0","cp_1"),
               names_to = "choice_option",
               names_prefix = "cp_",
               values_to = "choice_prob")

### remove dwell time
choice_prob <- subset(choice_prob, select = -c(dt_0, dt_1))


### combine long data frames
outcome <- merge(dwell_time, choice_prob, by = c("id", "session", "group", "choice_option"))

str(outcome)
outcome$choice_option <- as.factor(outcome$choice_option)


# Extend ggplot2's GeomViolin ----------

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


# Plot option dwell time ------- 

### create labeller for subgroups

group_labels <- as_labeller(c("control" = "Control",
                              "carbon_emissions" = "Carbon Emissions",
                              "operating_costs" = "Operating Costs",
                              "rating" = "Rating"))

dt_plot <- 
outcome %>%
  ggplot(mapping = aes(x = session, y = dwell_time, fill = choice_option)) +
  
  # violin blot
  geom_split_violin() +
  
  # box plot
  geom_boxplot(width = 0.1, position = position_dodge(0.2), show.legend = FALSE) +
  
  # layout
  facet_grid(rows = vars(group),
             labeller = group_labels) +
  
  # color
  scale_fill_manual(values = color_choice,
                    labels = c("0" = "Non-Eco Option",
                               "1" = "Eco Option")) +
  
  # axes and titles
  coord_cartesian(ylim = c(0, 15.5)) +
  labs(
    title = "Option Dwell Time",
    x = "Session",
    y = "Average Dwell Time (s)",
    fill = "Choice Option"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, 
                                  face = "bold", 
                                  hjust = 0.5,
                                  margin = margin(t = 0, r = 0, b = 10, l = 0)),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5, "cm"),
        legend.position = "top",
        plot.margin = margin(t = 10,
                             r = 10,
                             b = 10,
                             l = 10),
        panel.spacing = unit(0.7, "lines")
  )


# Plot choice probability ---------

cp_plot <- 
  outcome %>%
  ggplot(mapping = aes(x = session, y = choice_prob, fill = choice_option)) +
  
  # violin blot
  geom_split_violin() +
  
  # box plot
  geom_boxplot(width = 0.1, position = position_dodge(0.2), show.legend = FALSE) +
  
  # layout
  facet_grid(rows = vars(group),
             labeller = group_labels) +
  
  # color
  scale_fill_manual(values = color_choice,
                    labels = c("0" = "Non-Eco Option",
                               "1" = "Eco Option")) +
  
  # axes and titles
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Choice Probability",
    x = "Session",
    y = "Choice Probability",
    fill = "Choice Option"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, 
                                  face = "bold", 
                                  hjust = 0.5,
                                  margin = margin(t = 0, r = 0, b = 10, l = 0)),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5, "cm"),
        legend.position = "top",
        plot.margin = margin(t = 10,
                             r = 10,
                             b = 10,
                             l = 10),
        panel.spacing = unit(0.7, "lines")
  )


# Calculate attribute dwell time ---------

dt_attribute <- df %>%
  group_by(id, condition, session) %>%
  summarize(dt_price_0 = mean(c(t_price0, t_price_translation0), na.rm = TRUE),
            dt_price_1 = mean(c(t_price1, t_price_translation1), na.rm = TRUE),
            dt_sustain_0 = mean(c(t_consumption0, t_consumption_translation0), na.rm = TRUE),
            dt_sustain_1 = mean(c(t_consumption1, t_consumption_translation1), na.rm = TRUE),
            dt_popularity_0 = mean(t_popularity0),
            dt_popularity_1 = mean(t_popularity1))

### convert to long format

dt_attribute <- dt_attribute %>%
  pivot_longer(cols = c(dt_price_0:dt_popularity_1),
               names_to = c("attribute", "choice_option"),
               names_sep = "_",
               names_prefix = "dt_",
               values_to = "dwell_time")

### convert ms to s
dt_attribute$dwell_time <- dt_attribute$dwell_time/1000


### collapse subgroups across price attribute manipulation

dt_attribute <- dt_attribute %>%
  mutate(group = case_when(
    condition %in% c(1, 2) ~ "control",
    condition %in% c(3, 4) ~ "operating_costs",
    condition %in% c(5, 6) ~ "carbon_emissions",
    condition %in% c(7, 8) ~ "rating",
    TRUE ~ NA_character_  # handles any other cases
  ))

### remove condition
dt_attribute <- subset(dt_attribute, select = -condition)

### check structure

str(dt_attribute)

dt_attribute <- dt_attribute %>%
  mutate(group = factor(group, levels = c("control", 
                                          "carbon_emissions", 
                                          "operating_costs",
                                          "rating")))

dt_attribute <- dt_attribute %>%
  mutate(attribute = factor(attribute, levels = c("price", 
                                                  "sustain",
                                                  "popularity")))

dt_attribute$choice_option <- as.factor(dt_attribute$choice_option)


# Plot attribute dwell time --------

attribute_labels <- as_labeller(c("price" = "Price",
                              "sustain" = "Sustainability",
                              "popularity" = "Popularity"))

dt_attribute_plot <- 
  dt_attribute %>%
  ggplot(mapping = aes(x = session, y = dwell_time, fill = choice_option)) +
  
  # violin blot
  geom_split_violin() +
  
  # box plot
  geom_boxplot(width = 0.1, position = position_dodge(0.2), show.legend = FALSE) +
  
  # layout
  facet_grid(rows = vars(group),
             cols = vars(attribute),
             labeller = labeller(group = group_labels,
                                 attribute = attribute_labels)) +
  
  # color
  scale_fill_manual(values = color_choice,
                    labels = c("0" = "Non-Eco Option",
                               "1" = "Eco Option")) +
  
  # axes and titles
  coord_cartesian(ylim = c(0, 7.5)) +
  labs(
    title = "Attribute Dwell Time",
    x = "Session",
    y = "Average Dwell Time (s)",
    fill = "Choice Option"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, 
                                  face = "bold", 
                                  hjust = 0.5,
                                  margin = margin(t = 0, r = 0, b = 10, l = 0)),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5, "cm"),
        legend.position = "top",
        plot.margin = margin(t = 10,
                             r = 50,
                             b = 10,
                             l = 50),
        panel.spacing = unit(0.7, "lines")
  )


# Combine plots ---------

combined_plot <- 
grid.arrange(cp_plot, dt_plot,
             dt_attribute_plot,
             ncol = 2, nrow = 2,
             layout_matrix = rbind(c(1,2), c(3,3)))

### add labels to combined plot

figure_1 <- as_ggplot(combined_plot) +
  draw_plot_label(label = c("a", "b", "c"), size = 20,
                  x = c(0, 0.5, 0), y = c(1, 1, 0.5))

# save plot
ggsave("figures/figure_1.png", 
       figure_1, 
       width = 12, 
       height = 16,
       bg = "white")










