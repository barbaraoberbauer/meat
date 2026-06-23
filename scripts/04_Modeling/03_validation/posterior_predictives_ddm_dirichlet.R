#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# purpose: simulate data for posterior predictives
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
              "rjags",
              "runjags")

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
library(rjags)
library(runjags)

# Load required modules

load.module("wiener")

rm(package, packages, is_package_installed)


### Load data ------

# specify subset of data 

dataset <- "original"
# datasets: "original", "replication"

translation_of_interest <- "environmental_friendliness"
# translations for original dataset: "control", "emissions", "operating_costs", "environmental_friendliness"
# translations for replication dataset: "control", "emission_add", "rating_add", "emission_replace"

time <- "20260424_0727"
# time stamp of data generation


# create filename


filename <- paste0("data/modeling/runJagsOutDDMDirichlet", "_", dataset, "_", translation_of_interest, "_", time, ".rds")


# Load modeling data
runJagsOut <- readRDS(filename)

rm(filename)

# Load behavioral data
load("data/behavior/preprocessedDataOriginal.RData")
load("data/behavior/preprocessedDataReplication.RData")

if (dataset == "original") {
  
  df <- dfOriginal
  
} else if (dataset == "replication") {
  
  df <- dfReplication
  
}

# set subset depending on condition

df_subset <- df %>%
  filter(consumption_translation == translation_of_interest)
  

# assign new ids that are starting from 1 and increment by 1
df_subset <- df_subset %>%
  mutate(id_new = dense_rank(id))

# sort data frame according to id_new (starting from 1 to last participant)
df_subset <- df_subset[order(df_subset$id_new),]

# sample size
SampleSize <- length(unique(df_subset$id_new))

### Combine chains ------

# store as mcmc object
mcmcfin = as.mcmc.list(runJagsOut)

# combine chains
combined_mcmcfin <- as.data.frame(do.call(rbind, mcmcfin))


# Prepare Data Simulation ------

simRuns <- 1000 # specify number of simulations

# write function that determines the index of the parameter
witch <- function(parameter_name){
  which(colnames(mcmcfin[[1]]) == parameter_name)
}

# initiate matrix to store simulation results
sim_results <- matrix(nrow = nrow(df_subset), ncol = simRuns)

# select model file
model_file <- "scripts/04_Modeling/bayes_models/simulation_DDM_dirichlet.txt"


# Simulate Data --------

wT <- matrix(, nrow = SampleSize, ncol = 3)
wT_AT <- matrix(, nrow = SampleSize, ncol = 3)

for (sim in 1:simRuns) {
  
  # draw random number from 1 to nRow(combined_mcmcfin) -> parameters from that iteration will be used to simulate data
  idx <- sample(1:nrow(combined_mcmcfin), 1)
  
  # initialize matrices for weights
  wT <- matrix(, nrow = SampleSize, ncol = 3)
  wT_AT <- matrix(, nrow = SampleSize, ncol = 3)
  
  # retrieve parameters
  wT[,1] <- unname(unlist(combined_mcmcfin[idx, witch("wT[1,1]") : (witch("wT[1,1]") + (SampleSize - 1))]))
  wT[,2] <- unname(unlist(combined_mcmcfin[idx, witch("wT[1,2]") : (witch("wT[1,2]") + (SampleSize - 1))]))
  wT[,3] <- unname(unlist(combined_mcmcfin[idx, witch("wT[1,3]") : (witch("wT[1,3]") + (SampleSize - 1))]))
  wT_AT[,1] <- unname(unlist(combined_mcmcfin[idx, witch("wT_AT[1,1]") : (witch("wT_AT[1,1]") + (SampleSize - 1))]))
  wT_AT[,2] <- unname(unlist(combined_mcmcfin[idx, witch("wT_AT[1,2]") : (witch("wT_AT[1,2]") + (SampleSize - 1))]))
  wT_AT[,3] <- unname(unlist(combined_mcmcfin[idx, witch("wT_AT[1,3]") : (witch("wT_AT[1,3]") + (SampleSize - 1))]))
  alpha <- unname(unlist(combined_mcmcfin[idx, witch("alpha[1]") : (witch("alpha[1]") + (SampleSize - 1))]))
  alpha_AT <- unname(unlist(combined_mcmcfin[idx, witch("alpha_AT[1]") : (witch("alpha_AT[1]") + (SampleSize - 1))]))
  scaling <- unname(unlist(combined_mcmcfin[idx, witch("scaling[1]") : (witch("scaling[1]") + (SampleSize - 1))]))
  scaling_AT <- unname(unlist(combined_mcmcfin[idx, witch("scaling_AT[1]") : (witch("scaling_AT[1]") + (SampleSize - 1))]))
  tau <- unname(unlist(combined_mcmcfin[idx, witch("tau[1]") : (witch("tau[1]") + (SampleSize - 1))]))
  tau_AT <- unname(unlist(combined_mcmcfin[idx, witch("tau_AT[1]") : (witch("tau_AT[1]") + (SampleSize - 1))]))
  sp <- unname(unlist(combined_mcmcfin[idx, witch("sp[1]") : (witch("sp[1]") + (SampleSize - 1))]))
  sp_AT <- unname(unlist(combined_mcmcfin[idx, witch("sp_AT[1]") : (witch("sp_AT[1]") + (SampleSize - 1))]))
  
  
  # create data list as input for JAGS (this time, parameters serve as input and RT are monitored)
  dat_sim <- list(N=nrow(df_subset),
                  Subject=df_subset$id_new,
                  Session=df_subset$session,
                  Price_Eco=df_subset$priceEco,
                  Energy_Eco=df_subset$energyEco,
                  Popularity_Eco=df_subset$popularityEco,
                  Price_NonEco=df_subset$priceNonEco,
                  Energy_NonEco=df_subset$energyNonEco,
                  Popularity_NonEco=df_subset$popularityNonEco,
                  
                  # parameters
                  wT = wT, 
                  wT_AT = wT_AT,
                  alpha=alpha, 
                  alpha_AT=alpha_AT, 
                  scaling=scaling, 
                  scaling_AT=scaling_AT, 
                  tau=tau, 
                  tau_AT=tau_AT,
                  sp=sp,
                  sp_AT=sp_AT
  )
  
  # declare variables to be monitored
  monitor_sim <- c("x")
  
  # simulate data using JAGS
  
  results <- run.jags(model = model_file,
                      monitor = monitor_sim,
                      module = "wiener",
                      data = dat_sim,
                      n.chains = 1,
                      sample = 1,
                      silent.jags = TRUE)

  sim_results[,sim] <- unlist(results$mcmc)
  
  # Print progress to console
  flush.console()
  msg = sprintf('Done with simulation: %d',sim)
  print(msg)
  
}

### Save simulated data

filename <- paste0("data/modeling/simResultsDDMDirichlet", "_", dataset, "_", translation_of_interest, "_", time, ".rds")

saveRDS(sim_results, file = filename)










