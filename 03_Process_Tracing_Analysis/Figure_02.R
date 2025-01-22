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

df <- readRDS("00_Data/df.rds")


### Load color paletts ----------

color_choice <- c("#9fc8c8", "#1f6f6f") # non-eco, eco choice
color_session <- c("#ced4da", "#8e9aaf") # session 1, session 2


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


# Calculate fixation proportions (fixprops) ---------

# fixProps -> acquistion time for each attribute proportional to the total duration of the trial (vector containing six elements in our case)
fixProps <- data.frame(price0 = rep(NA, nrow(df)),
                       consumption0 = rep(NA, nrow(df)),
                       popularity0 = rep(NA, nrow(df)),
                       price1 = rep(NA, nrow(df)),
                       consumption1 = rep(NA, nrow(df)),
                       popularity1 = rep(NA, nrow(df))) 

# attributes and their translation are treated as one attribute for simplicity
# divided by 1000 to transform into seconds
fixProps$price0 <- rowSums(df[, c("t_price0", "t_price_translation0")], na.rm = TRUE)/1000
fixProps$consumption0 <- rowSums(df[, c("t_consumption0", "t_consumption_translation0")], na.rm = TRUE)/1000
fixProps$popularity0 <- df$t_popularity0/1000
fixProps$price1 <- rowSums(df[, c("t_price1", "t_price_translation1")], na.rm = TRUE)/1000
fixProps$consumption1 <- rowSums(df[, c("t_consumption1", "t_consumption_translation1")], na.rm = TRUE)/1000
fixProps$popularity1 <- df$t_popularity1/1000

# divide by total duration of the trial
fixProps <- fixProps/(df$t_decision/1000) 

# normalize each trial to 1
# try without normalizing first as normalizing might hide trends of not looking at all
#fixProps <- fixProps/rowSums(fixProps)

# round to two decimals after the comma
fixProps <- round(fixProps, 2)

# add id, session, and group information to fixProps
fixProps$id <- df$id
fixProps$session <- df$session
fixProps$group <- df$consumption_translation


# Aggregate fixation proportions --------

fixProps_agg <- fixProps %>%
  group_by(id, session, group) %>%
  summarize(price_0 = mean(price0),
            price_1 = mean(price1),
            consumption_0 = mean(consumption0),
            consumption_1 = mean(consumption1),
            popularity_0 = mean(popularity0),
            popularity_1 = mean(popularity1))

### convert to long format

fixProps_agg <- fixProps_agg %>%
  pivot_longer(cols = c(price_0:popularity_1),
               names_to = c("attribute", "choice_option"),
               names_sep = "_",
               values_to = "fixProps")

### check structure of data frame

str(fixProps_agg)

fixProps_agg <- fixProps_agg %>%
  mutate(group = factor(group, levels = c("control", 
                                          "emissions", 
                                          "operating_costs",
                                          "environmental_friendliness")))

fixProps_agg <- fixProps_agg %>%
  mutate(attribute = factor(attribute, levels = c("price", 
                                                  "consumption",
                                                  "popularity")))


# Plot fixation proportions --------

group_labels <- as_labeller(c("control" = "Control",
                              "emissions" = "Carbon Emissions",
                              "operating_costs" = "Operating Costs",
                              "environmental_friendliness" = "Rating"))


attribute_labels <- as_labeller(c("price" = "Price",
                                  "consumption" = "Sustainability",
                                  "popularity" = "Popularity"))

fixProps_plot <- 
  fixProps_agg %>%
  ggplot(mapping = aes(x = session, y = fixProps, fill = choice_option)) +
  
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
  coord_cartesian(ylim = c(0, 0.5)) +
  labs(
    title = "Proportional Dwell Time",
    x = "Session",
    y = "Dwell Time proportional to Trial Duration",
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
  geom_split_violin() +
  
  # box plot
  geom_boxplot(width = 0.1, position = position_dodge(0.2), show.legend = FALSE) +
  
  # color
  scale_fill_manual(values = color_session,
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


# Calculate relative dwell times for attributes -------

# calculate as (t_price_1 - t_price_0) / (t_price_1 + t_price_0)

df$diff_t_price <- (rowSums(df[, c("t_price1", "t_price_translation1")], na.rm = TRUE) -
                      rowSums(df[, c("t_price0", "t_price_translation0")], na.rm = TRUE)) /
  (rowSums(df[, c("t_price1", "t_price_translation1")], na.rm = TRUE) +
     rowSums(df[, c("t_price0", "t_price_translation0")], na.rm = TRUE))

df$diff_t_consumption <- (rowSums(df[, c("t_consumption1", "t_consumption_translation1")], na.rm = TRUE) -
                      rowSums(df[, c("t_consumption0", "t_consumption_translation0")], na.rm = TRUE)) /
  (rowSums(df[, c("t_consumption1", "t_consumption_translation1")], na.rm = TRUE) +
     rowSums(df[, c("t_consumption0", "t_consumption_translation0")], na.rm = TRUE))

df$diff_t_popularity <- (rowSums(df[, c("t_popularity1")], na.rm = TRUE) -
                           rowSums(df[, c("t_popularity0")], na.rm = TRUE)) /
  (rowSums(df[, c("t_popularity1")], na.rm = TRUE) +
     rowSums(df[, c("t_popularity0")], na.rm = TRUE))

# NaNs result when participants do not inspect an attribute on a trial


# extract relevant data and create new data frame

dt_attributes <- df %>%
  select(id, 
         session, 
         consumption_translation, 
         diff_t_price, 
         diff_t_consumption, 
         diff_t_popularity)

### convert to long format

dt_attributes <- dt_attributes %>%
  pivot_longer(cols = c(diff_t_price:diff_t_popularity),
               names_to = "attribute",
               names_prefix = "diff_t_",
               values_to = "diff_t")

### check structure of data frame

str(dt_attributes)

dt_attributes <- dt_attributes %>%
  mutate(consumption_translation = factor(consumption_translation, levels = c("control", 
                                          "emissions", 
                                          "operating_costs",
                                          "environmental_friendliness")))

dt_attributes <- dt_attributes %>%
  mutate(attribute = factor(attribute, levels = c("price", 
                                                  "consumption",
                                                  "popularity")))



# Plot relative dwell times for attributes -------

group_labels <- as_labeller(c("control" = "Control",
                              "emissions" = "Carbon Emissions",
                              "operating_costs" = "Operating Costs",
                              "environmental_friendliness" = "Rating"))

dt_attribute_plot <- 
  dt_attributes %>%
  ggplot(mapping = aes(x = attribute, y = diff_t, fill = session)) +
  
  # violin blot
  geom_split_violin() +
  
  # box plot
  geom_boxplot(width = 0.1, position = position_dodge(0.2), show.legend = FALSE) +
  
  # layout
  facet_grid(rows = vars(consumption_translation),
             labeller = group_labels) +
  
  # color
  scale_fill_manual(values = color_session,
                    labels = c("1" = "1",
                               "2" = "2")) +
  
  # axes and titles
  coord_cartesian(ylim = c(-1, 1)) +
  labs(
    title = "Differential Dwell Time - Attributes",
    x = "Attribute",
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

