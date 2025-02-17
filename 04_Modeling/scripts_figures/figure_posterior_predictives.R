#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# last update: "2025-01-23"
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
              "cowplot",
              "bayestestR")

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
library(bayestestR)

# Load required functions
source("functions/fun_plot_posterior_predictives.R")


rm(package, packages, is_package_installed)


### Load data ------

# specify subset of data 

group_of_interest <- "environmental_friendliness"
# groups: "control", "emissions", "operating_costs", "environmental_friendliness"

# bounded or unbounded attentional parameters? 

bounded <- FALSE # set to TRUE for bounded model, set to FALSE for unbounded model

if (bounded == TRUE) {
  
  file_extension <- "_bounds"
  
} else if (bounded == FALSE) {
  
  file_extension <- "_nobounds"
  
}

filename <- paste0("data/simResults_", group_of_interest, file_extension, ".rds")

simResults <- readRDS(filename)

rm(filename)

df <- readRDS("data/df.rds")

df_subset <- df %>%
  filter(consumption_translation == group_of_interest)


# Calculate Bayesian Credibility Intervals for each Bin of Plot ----------

### Preparations

# calculate max empirical response time to determine bins
maxRT <- (max(df_subset$t_decision)/1000) + 1 #extend max response time so that max RT is also in bin


# determine binwidth
binwidth <- 1.4

# create breaks
breaks <- seq(0, 
              maxRT, 
              by = binwidth)


# calculate frequencies
frequency <- df_subset %>%
  mutate(bins = cut(t_decision/1000, breaks = breaks, include.lowest = TRUE)) %>%
  group_by(session, choice, bins) %>%
  summarize(count = n(), .groups = 'drop') %>%
  ungroup() %>%
  complete(session, choice, bins, fill = list(count = 0))

# add mid of bins to frequency
# calculate mid bins
mid_bins <- seq(binwidth/2, length.out = length(breaks)-1, by = binwidth)
frequency$mid_bins <- rep(mid_bins, 4)

# set up data frame to store frequencies from simulation runs
frequencies_sim <- frequency %>%
  select("session", "choice", "bins", "mid_bins")


### Calculate Frequencies for Simulated Data -----

# loop through all runs

for (run in 1:ncol(simResults)) {
  
  # add choice and session info
  subset_sim <- as.data.frame(simResults[,run])
  names(subset_sim)[1] <- "t_decision"
  subset_sim$choice <- ifelse(subset_sim$t_decision > 0, 1, 0)
  subset_sim$t_decision <- abs(subset_sim$t_decision)
  subset_sim$session <- df_subset$session
  
  # calculate frequencies
  subset_frequency_sim <- subset_sim %>%
    mutate(bins = cut(abs(t_decision), breaks = breaks, include.lowest = TRUE)) %>%
    group_by(session, choice, bins) %>%
    summarize(count = n(), .groups = 'drop') %>%
    ungroup() %>%
    complete(session, choice, bins, fill = list(count = 0))
  
  # add to frequencies_sim data frame
  frequencies_sim <- frequencies_sim %>%
    left_join(subset_frequency_sim,
              by = c("session", "choice", "bins"))
  
  # rename column
  colnames(frequencies_sim)[ncol(frequencies_sim)] <- paste0("sim_", run)
  
}

### Calculate BCIs ---------

###### Equal-tail BCIs ------

# Define the boundaries for quantiles
# lower_boundary <- 0.025
# upper_boundary <- 0.975
# 
# # calculate bcis for each row
# frequencies_sim$lower_bci <- apply(frequencies_sim[,5:ncol(frequencies_sim)],
#                                    1,
#                                    function(row) quantile(row, lower_boundary, na.rm = FALSE))
# 
# frequencies_sim$upper_bci <- apply(frequencies_sim[,5:ncol(frequencies_sim)],
#                                    1,
#                                    function(row) quantile(row, upper_boundary, na.rm = FALSE))
# 
# # add lower and upper bci to empirical data (frequency)
# frequency <- frequency %>%
#   left_join(frequencies_sim %>%
#               select(session, choice, bins, lower_bci, upper_bci), 
#             by = c("session", "choice", "bins"))
# 
# rm(binwidth, breaks, lower_boundary, upper_boundary, maxRT, run)

###### HDIs --------

# convert to numeric matrix
frequencies_sim[, 5:ncol(frequencies_sim)] <- sapply(frequencies_sim[, 5:ncol(frequencies_sim)], as.numeric)


HDIs <- apply(frequencies_sim[,5:ncol(frequencies_sim)],
                                   1,
                                   FUN = function(row) bayestestR::hdi(row, verbose = FALSE))

lower_CI <- unlist(sapply(HDIs, function(x) x["CI_low"]))
upper_CI <- unlist(sapply(HDIs, function(x) x["CI_high"]))

frequencies_sim$lower_CI <- lower_CI
frequencies_sim$upper_CI <- upper_CI

# add lower and upper bci to empirical data (frequency)
frequency <- frequency %>%
  left_join(frequencies_sim %>%
              select(session, choice, bins, lower_CI, upper_CI), 
            by = c("session", "choice", "bins"))


# Plot Frequencies of Empirical and Simulated Data ------

### Set Prerequisites -----

# set colors
color_choice <- c("#CBCBD4", "#556F44") # non-eco, eco choice
color_error <- '#cb181d'

# rename count column
names(frequency)[4] <- "count_emp"

### Plot Sim and Emp Data for both Sessions ------

post_pred_session_1 <- plot_posterior_predictives(1, "Baseline Session")

post_pred_session_2 <- plot_posterior_predictives(2, "Manipulation Session")

### Arrange Plots in Grid
posterior_predictives <- plot_grid(# plots
  post_pred_session_1,
  post_pred_session_2,
  
  # settings
  ncol = 2,
  labels = c("a", "b")
)

# save plot
filename <- paste0("figures/posterior_predictives_", group_of_interest, file_extension, ".png")
ggsave(filename, posterior_predictives, width = 12, height = 5)
