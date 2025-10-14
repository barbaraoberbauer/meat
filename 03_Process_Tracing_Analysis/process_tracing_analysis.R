#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# last update: "2025-01-15"
# produced under R version: 2024.09.0
#---

# Process tracing analysis

# Load packages and read data ------------

#clear working environment
rm(list=ls())

#clear all plots
if(!is.null(dev.list())) dev.off()


### Install packages -------

# List of packages to check and install if necessary
packages <- c("tidyverse", "psych", "ggplot2")

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
library(psych)
library(ggplot2)

rm(package, packages, is_package_installed)


### Load data ---------

df <- readRDS("data/df.rds")

# Probability Choosing Eco - Dwell Time Differences --------

# set translation of interest
translation_of_interest <- "environmental_friendliness"

# add dwell time differences to data frame
df$ddt_eco_noneco <- (df$t_option1 - df$t_option0)/1000 # in sec

# subset data
df_subset <- df %>%
  filter(consumption_translation == translation_of_interest)

### Summarize empirical data -----

# only include the central 95% of dwell time data
bounds <- df_subset %>%
  group_by(session) %>%
  summarize(
    lower_bound = quantile(ddt_eco_noneco, 0.025, na.rm = TRUE),
    upper_bound = quantile(ddt_eco_noneco, 0.975, na.rm = TRUE)
  )

# filter data
filtered_data <- df_subset %>%
  left_join(bounds, by = "session") %>%
  filter(ddt_eco_noneco >= lower_bound & ddt_eco_noneco <= upper_bound)


# create breakpoints
break_points <- seq(from = round(min(filtered_data$ddt_eco_noneco)),
                    to = round(max(filtered_data$ddt_eco_noneco)),
                    by = 1)

#mid_points <- break_points[-length(break_points)] + diff(break_points) / 2

chooseEcoProbability <- filtered_data %>%
  group_by(session) %>%
  mutate(bins = cut(ddt_eco_noneco, 
                    breaks = break_points, 
                    include.lowest = TRUE)) %>% 
  group_by(session, bins) %>%
  summarize(binMean = mean(ddt_eco_noneco),
            ecoProb = mean(choice == 1),
            seEcoProb = sqrt(mean(choice == 1) * (1 - mean(choice == 1)) / n()))

### Plot data ------

# session colors
cols_sess <- c("#225780", "#8CC5E3")

ggplot(chooseEcoProbability, aes(x = binMean, y = ecoProb, color = session)) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.7) +
  geom_errorbar(aes(ymin = ecoProb - 2 * seEcoProb, 
                    ymax = ecoProb + 2 * seEcoProb),
                linewidth = 1) +
  scale_color_manual(values = cols_sess) +
  labs(x = "Dwell time difference (eco - other) [s]", 
       y = "P(Eco Choice)", 
       color = "Session") +
  theme_bw() +
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        plot.margin = margin(t = 15,
                             r = 15,
                             b = 15,
                             l = 15),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size = 13),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), size = 13),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.key.size = unit(0.5, "cm"),
        legend.position = "top"
        
  )


