#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# purpose: plot posterior predictives
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
source("R/functions/fun_plot_posterior_predictives.R")

# Load theme
source("R/theme.R")
theme_set(themeMEAT())

rm(package, packages, is_package_installed)


### Load data ------

load("data/preprocessedDataOriginal.RData")
load("data/preprocessedDataReplication.RData")

### Specify subset of data ----

dataset <- "replication"
# datasets: "original", "replication"

translation_of_interest <- "rating_add"
# translations for original dataset: "control", "emissions", "operating_costs", "environmental_friendliness"
# translations for replication dataset: "control", "emission_add", "rating_add", "emission_replace"

run_subgroups_separately <- TRUE
# if set to TRUE, estimates parameters separately for participants that did receive an additional price translation at t2 and those who did not
# only applicable to original data

group_of_interest <- "price_translation_present"
# groups: "price_translation_absent", "price_translation_present"
# only applicable to original data

time <- "20260325_0851"
# time stamp of data generation

# bounded or unbounded attentional parameters? 
bounded <- FALSE # set to TRUE for bounded model, set to FALSE for unbounded model

if (bounded == TRUE) {
  
  file_extension <- "bounds"
  
} else if (bounded == FALSE) {
  
  file_extension <- "nobounds"
  
}

# simulated data

filename <- paste0("data/modeling/simResults", "_", dataset, "_", translation_of_interest, "_", time, ".rds")
simResults <- readRDS(filename)

rm(filename)


# empirical data

if (dataset == "original") {
  
  df <- dfOriginal
  
} else if (dataset == "replication") {
  
  df <- dfReplication
  
}

df_subset <- df %>%
  filter(consumption_translation == translation_of_interest)

# assign new ids that are starting from 1 and increment by 1
df_subset <- df_subset %>%
  mutate(id_new = dense_rank(id))

# sort data frame according to id_new (starting from 1 to last participant)
df_subset <- df_subset[order(df_subset$id_new),]


# Calculate Bayesian Credibility Intervals for each Bin of Plot ----------

### Preparations

# calculate max empirical response time to determine bins
maxRT <- (max(df_subset$t_decision)/1000) + 2 #extend max response time so that max RT is also in bin


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

# rename count column
names(frequency)[4] <- "count_emp"

# plot only subset of frequeny (get rid of long tails that model expects)
frequency <- subset(frequency, !(count_emp < 1 & mid_bins > 10))

### Set Prerequisites -----

### Plot Sim and Emp Data for both Sessions ------

post_pred_session_1 <- plot_posterior_predictives(1, "Session 1")

post_pred_session_2 <- plot_posterior_predictives(2, "Session 2")

### Arrange Plots in Grid
posterior_predictives <- plot_grid(# plots
  post_pred_session_1 + theme(legend.position = "none"),
  post_pred_session_2 + theme(legend.position = "none"),
  
  # settings
  ncol = 2,
  labels = c("a", "b"),
  label_size = 20
)

# get legend
legend <- get_legend(post_pred_session_1 + 
                       guides(color = guide_legend(nrow = 1)) +
                       theme(legend.position = "bottom"))


posterior_predictives <- plot_grid(posterior_predictives,
                                   legend,
                                   ncol = 1,
                                   rel_heights = c(1, .1))


# save plot
filename <- paste0("figures/posterior_predictives", "_", dataset, "_", translation_of_interest, "_", file_extension, "_", time, ".png")
ggsave(filename, posterior_predictives, width = 12, height = 6)
