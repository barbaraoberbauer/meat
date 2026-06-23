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
              "patchwork",
              "grid",
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
library(patchwork)
library(grid)
library(bayestestR)

# Load required functions
source("R/functions/fun_plot_posterior_predictives.R")

# Load theme
source("R/theme.R")
theme_set(themeMEAT())

rm(package, packages, is_package_installed)


### Load data ------

load("data/behavior/preprocessedDataOriginal.RData")
load("data/behavior/preprocessedDataReplication.RData")

### Specify subset of data ----

# specify subset of data 

# translations of interest: control, emissions, environmental_friendliness (these conditions are identical across studies)
translation_of_interest <- "environmental_friendliness"

# time stamp of data generation
time_original <- "20260519_0532"
time_replication <- "20260518_2319"


# set names of conditions for each study
if (translation_of_interest == "environmental_friendliness") {
  
  toi_original <- "environmental_friendliness"
  toi_replication <- "rating_add"
  
  xtitleChangePlot <- "Effect of Attr. Transl."
  
} else if (translation_of_interest == "emissions") {
  
  toi_original <- "emissions"
  toi_replication <- "emissions_add"
  
  xtitleChangePlot <- "Effect of Attr. Transl."
  
} else if (translation_of_interest == "control") {
  
  toi_original <- "control"
  toi_replication <- "control"
  
  xtitleChangePlot <- "Difference btw. Sessions"
  
}

# simulated data
filenameOriginal <- paste0("data/modeling/simResultsmaaDDMDirichlet_original", "_", 
                           toi_original, "_", 
                           time_original, ".rds")

filenameReplication <- paste0("data/modeling/simResultsmaaDDMDirichlet_replication", "_", 
                              toi_replication, "_", 
                              time_replication, ".rds")

simResultsOriginal <- readRDS(filenameOriginal)
simResultsReplication <- readRDS(filenameReplication)

rm(filenameOriginal, filenameReplication)

# Calculate Bayesian Credibility Intervals for each Bin of Plot ----------

calculate_frequencies <- function(df, simResults, toi){
  
  df_subset <- df %>%
    filter(consumption_translation == toi) # translation of interest
  
  # assign new ids that are starting from 1 and increment by 1
  df_subset <- df_subset %>%
    mutate(id_new = dense_rank(id))
  
  # sort data frame according to id_new (starting from 1 to last participant)
  df_subset <- df_subset[order(df_subset$id_new),]
  
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
  
  return(list(
    frequencies_empirical = frequency,
    frequencies_simulated = frequencies_sim
  ))
  

}

frequenciesOriginal <- calculate_frequencies(dfOriginal,
                                             simResultsOriginal,
                                             toi_original)

frequenciesReplication <- calculate_frequencies(dfReplication,
                                             simResultsReplication,
                                             toi_replication)


# Get HDIs --------

calculate_HDIs <- function(frequencies_emp, frequencies_sim){
  
  # convert to numeric matrix
  frequencies_sim[, 5:ncol(frequencies_sim)] <- 
    sapply(frequencies_sim[, 5:ncol(frequencies_sim)], as.numeric)
  
  HDIs <- apply(frequencies_sim[,5:ncol(frequencies_sim)],
                1,
                FUN = function(row) bayestestR::hdi(row, verbose = FALSE))
  
  lower_CI <- unlist(sapply(HDIs, function(x) x["CI_low"]))
  upper_CI <- unlist(sapply(HDIs, function(x) x["CI_high"]))
  
  frequencies_sim$lower_CI <- lower_CI
  frequencies_sim$upper_CI <- upper_CI
  
  # add lower and upper bci to empirical data (frequency)
  frequencies <- frequencies_emp %>%
    left_join(frequencies_sim %>%
                select(session, choice, bins, lower_CI, upper_CI), 
              by = c("session", "choice", "bins"))
  
  # rename count column
  names(frequencies)[4] <- "count_emp"
  
  return(frequencies)
  
}

combinedFrequenciesOriginal <- calculate_HDIs(frequenciesOriginal$frequencies_empirical,
                                              frequenciesOriginal$frequencies_simulated)

combinedFrequenciesReplication <- calculate_HDIs(frequenciesReplication$frequencies_empirical,
                                              frequenciesReplication$frequencies_simulated)


# Plot Frequencies of Empirical and Simulated Data ------

# plot only subset of frequeny (get rid of long tails that model expects)
combinedFrequenciesOriginal <- subset(combinedFrequenciesOriginal, !(count_emp < 1 & mid_bins > 10))


combinedFrequenciesReplication <- subset(combinedFrequenciesReplication, !(count_emp < 1 & mid_bins > 10))

max(combinedFrequenciesReplication$mid_bins)

# generate plots

maxRT <- max(max(combinedFrequenciesOriginal$mid_bins),
             max(combinedFrequenciesReplication$mid_bins))

ppOriginalSession1 <- plot_posterior_predictives(combinedFrequenciesOriginal, 
                                                 1, 
                                                 maxRT,
                                                 "Session 1") + 
  theme(axis.title.x = element_blank())

ppOriginalSession2 <- plot_posterior_predictives(combinedFrequenciesOriginal, 
                                                 2, 
                                                 maxRT,
                                                 "Session 2") + 
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())

ppReplicationSession1 <- plot_posterior_predictives(combinedFrequenciesReplication, 
                                                 1, 
                                                 maxRT,
                                                 "Session 1") + 
  theme(plot.title = element_blank())

ppReplicationSession2 <- plot_posterior_predictives(combinedFrequenciesReplication, 
                                                 2, 
                                                 maxRT,
                                                 "Session 2") + 
  theme(plot.title = element_blank(),
        axis.title.y = element_blank())


# Combine plots ------

# create header

original_label <- wrap_elements(
  grid::textGrob("Study 1", 
                 rot = 90,
                 #hjust = 0.5,
                 #vjust = 1,  
                 gp = grid::gpar(fontsize = 20, fontface = "bold"))
)

replication_label <- wrap_elements(
  grid::textGrob("Study 2",
                 rot = 90,
                 #hjust = 0.5,
                 #vjust = 1,  
                 gp = grid::gpar(fontsize = 20, fontface = "bold"))
)


# arrange plots

pps <- original_label + ppOriginalSession1 + ppOriginalSession2 +
  replication_label + ppReplicationSession1 + ppReplicationSession2 +
  plot_layout(ncol = 3,
              widths = c(0.15, 1, 1),
              guides = 'collect') &
  theme(legend.position = 'bottom',
        plot.margin = margin(t = 7, r = 7, b = 7, l = 7),
        panel.border = element_rect(color = "black", fill = NA))


# Save plot
filename <- paste0("figures/posteriorPredictivesBothStudies", "_", translation_of_interest, ".png")
ggsave(filename, pps, width = 10, height = 7)
