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
library(rtdists)

# Load required functions
source("functions/fun_sim_maaDDM.R")

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

filename <- paste0("data/runJagsOut_", group_of_interest, file_extension, ".rds")

runJagsOut <- readRDS(filename)

rm(filename)

df <- readRDS("data/df.rds")

df_subset <- df %>%
  filter(consumption_translation == group_of_interest)

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


# Calculate fixation proportions (fixprops) ------

# fixProps -> acquistion time for each attribute proportional to the total duration of the trial (vector containing six elements in our case)
fixProps <- data.frame(price0 = rep(NA, nrow(df_subset)),
                       consumption0 = rep(NA, nrow(df_subset)),
                       popularity0 = rep(NA, nrow(df_subset)),
                       price1 = rep(NA, nrow(df_subset)),
                       consumption1 = rep(NA, nrow(df_subset)),
                       popularity1 = rep(NA, nrow(df_subset))) 

# attributes and their translation are treated as one attribute for simplicity
fixProps$price0 <- rowSums(df_subset[, c("t_price0", "t_price_translation0")], na.rm = TRUE)/1000
fixProps$consumption0 <- rowSums(df_subset[, c("t_consumption0", "t_consumption_translation0")], na.rm = TRUE)/1000
fixProps$popularity0 <- df_subset$t_popularity0/1000
fixProps$price1 <- rowSums(df_subset[, c("t_price1", "t_price_translation1")], na.rm = TRUE)/1000
fixProps$consumption1 <- rowSums(df_subset[, c("t_consumption1", "t_consumption_translation1")], na.rm = TRUE)/1000
fixProps$popularity1 <- df_subset$t_popularity1/1000

# divide by total duration of the trial
fixProps <- fixProps/abs(df_subset$t_decision/1000) #take absolute value instead of +/- coded RT

# normalize each trial to 1
fixProps <- fixProps/rowSums(fixProps)

# Prepare Data Simulation ------

simRuns <- 100 # specify number of simulations

# write function that determines the index of the parameter
witch <- function(parameter_name){
  which(colnames(mcmcfin[[1]]) == parameter_name)
}

# define static inputs for function
N <- nrow(df_subset)
Subject <- df_subset$id_new
Session <- df_subset$session
Price_Eco <- df_subset$priceEco
Energy_Eco <- df_subset$energyEco
Popularity_Eco <- df_subset$popularityEco
Price_NonEco <- df_subset$priceNonEco
Energy_NonEco <- df_subset$energyNonEco
Popularity_NonEco <- df_subset$popularityNonEco

# define data frame to store results
sim_results <- as.data.frame(matrix(nrow = N, ncol = simRuns))

# Simulate Data -------

for (sim in 1:simRuns) {
  
  ### draw random number from 1 to nRow(combined_mcmcfin) -> parameters from that iteration will be used to simulate data
  idx <- sample(1:nrow(combined_mcmcfin), 1)
  
  ### inputs for function
  w1T <- unname(unlist(combined_mcmcfin[idx, witch("w1T[1]") : (witch("w1T[1]") + (SampleSize - 1))]))
  w1T_AT <- unname(unlist(combined_mcmcfin[idx, witch("w1T_AT[1]") : (witch("w1T_AT[1]") + (SampleSize - 1))]))
  w2T <- unname(unlist(combined_mcmcfin[idx, witch("w2T[1]") : (witch("w2T[1]") + (SampleSize - 1))]))
  w2T_AT <- unname(unlist(combined_mcmcfin[idx, witch("w2T_AT[1]") : (witch("w2T_AT[1]") + (SampleSize - 1))]))
  w3T <- unname(unlist(combined_mcmcfin[idx, witch("w3T[1]") : (witch("w3T[1]") + (SampleSize - 1))]))
  w3T_AT <- unname(unlist(combined_mcmcfin[idx, witch("w3T_AT[1]") : (witch("w3T_AT[1]") + (SampleSize - 1))]))
  thetaT <- unname(unlist(combined_mcmcfin[idx, witch("thetaT[1]") : (witch("thetaT[1]") + (SampleSize - 1))]))
  thetaT_AT <- unname(unlist(combined_mcmcfin[idx, witch("thetaT_AT[1]") : (witch("thetaT_AT[1]") + (SampleSize - 1))]))
  phiT <- unname(unlist(combined_mcmcfin[idx, witch("phiT[1]") : (witch("phiT[1]") + (SampleSize - 1))]))
  phiT_AT <- unname(unlist(combined_mcmcfin[idx, witch("phiT_AT[1]") : (witch("phiT_AT[1]") + (SampleSize - 1))]))
  alpha <- unname(unlist(combined_mcmcfin[idx, witch("alpha[1]") : (witch("alpha[1]") + (SampleSize - 1))]))
  alpha_AT <- unname(unlist(combined_mcmcfin[idx, witch("alpha_AT[1]") : (witch("alpha_AT[1]") + (SampleSize - 1))]))
  scaling <- unname(unlist(combined_mcmcfin[idx, witch("scaling[1]") : (witch("scaling[1]") + (SampleSize - 1))]))
  scaling_AT <- unname(unlist(combined_mcmcfin[idx, witch("scaling_AT[1]") : (witch("scaling_AT[1]") + (SampleSize - 1))]))
  tau <- unname(unlist(combined_mcmcfin[idx, witch("tau[1]") : (witch("tau[1]") + (SampleSize - 1))]))
  tau_AT <- unname(unlist(combined_mcmcfin[idx, witch("tau_AT[1]") : (witch("tau_AT[1]") + (SampleSize - 1))]))
  sp <- unname(unlist(combined_mcmcfin[idx, witch("sp[1]") : (witch("sp[1]") + (SampleSize - 1))]))
  sp_AT <- unname(unlist(combined_mcmcfin[idx, witch("sp_AT[1]") : (witch("sp_AT[1]") + (SampleSize - 1))]))
  
  result <- sim_maaDDM(N,
                       Subject,
                       Session,
                       Price_Eco,
                       Energy_Eco,
                       Popularity_Eco,
                       Price_NonEco,
                       Energy_NonEco,
                       Popularity_NonEco,
                       fixProps,
                       
                       # parameters
                       w1T, 
                       w1T_AT,
                       w2T,
                       w2T_AT,
                       w3T,
                       w3T_AT, 
                       thetaT,
                       thetaT_AT, 
                       phiT, 
                       phiT_AT, 
                       alpha, 
                       alpha_AT, 
                       scaling, 
                       scaling_AT, 
                       tau, 
                       tau_AT,
                       sp,
                       sp_AT)
  
  ### store results in data frame
  sim_results[,sim] <- result
  
  ### print progress to console
  flush.console()
  msg = sprintf('Done with sim run: %d',sim)
  print(msg)
      

}

### Save simulated data

filename <- paste0("data/simResults_", group_of_interest, file_extension, ".rds")
saveRDS(sim_results, file = filename)
