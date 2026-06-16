#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
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
              "parallel",
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
library(parallel)

# Load required modules
load.module("wiener")

# Load functions written by John Kruschke
# https://github.com/boboppie/kruschke-doing_bayesian_data_analysis/blob/master/2e/DBDA2E-utilities.R
source("R/functions/DBDA2E-utilities.R")

rm(package, packages, is_package_installed)


### Load data ------

# specify subset of data 

dataset <- "replication"
# datasets: "original", "replication"

translation_of_interest <- "rating_add"
# translations for original dataset: "control", "emissions", "operating_costs", "environmental_friendliness"
# translations for replication dataset: "control", "emission_add", "rating_add", "emission_replace"

time <- "20260518_2319"
# time stamp of data generation


filename <- paste0("data/modeling/runJagsOutmaaDDMDirichlet_", dataset, "_", translation_of_interest, "_", time, ".rds")

runJagsOut_true <- readRDS(filename)

rm(filename)

# load behavioral data

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
mcmcfin_true = as.mcmc.list(runJagsOut_true)

# combine chains
combined_mcmcfin_true <- as.data.frame(do.call(rbind, mcmcfin_true))


# Calculate fixation proportions (fixprops) ------

### Calculate fixation proportions (fixprops) ------

# fixProps -> acquistion time for each attribute proportional to the total duration of the trial (vector containing six elements in our case)
fixProps <- data.frame(price0 = rep(NA, nrow(df_subset)),
                       consumption0 = rep(NA, nrow(df_subset)),
                       popularity0 = rep(NA, nrow(df_subset)),
                       price1 = rep(NA, nrow(df_subset)),
                       consumption1 = rep(NA, nrow(df_subset)),
                       popularity1 = rep(NA, nrow(df_subset))) 

# attributes and their translation are treated as one attribute for simplicity
# depending on dataset, summarize price and price translation
if (dataset == "original") {
  
  fixProps$price0 <- rowSums(df_subset[, c("t_price0", "t_price_translation0")], na.rm = TRUE)/1000
  fixProps$price1 <- rowSums(df_subset[, c("t_price1", "t_price_translation1")], na.rm = TRUE)/1000
  
} else if (dataset == "replication") {
  
  fixProps$price0 <- df_subset$t_price0/1000
  fixProps$price1 <- df_subset$t_price1/1000
  
}

fixProps$consumption0 <- rowSums(df_subset[, c("t_consumption0", "t_consumption_translation0")], na.rm = TRUE)/1000
fixProps$popularity0 <- df_subset$t_popularity0/1000
fixProps$consumption1 <- rowSums(df_subset[, c("t_consumption1", "t_consumption_translation1")], na.rm = TRUE)/1000
fixProps$popularity1 <- df_subset$t_popularity1/1000

# divide by total duration of the trial
#fixProps <- fixProps/abs(df_subset$t_decision) #take absolute value instead of +/- coded RT
fixProps <- fixProps/df_subset$t_total # divide by total dwell time

# normalize each trial to 1
fixProps <- fixProps/rowSums(fixProps) 


# Determine best fitting parameters (highest log-likelihood) ------------

# likelihood of an observation given the parameters

mcmcMat <- as.matrix(mcmcfin_true,chains=TRUE)
mcmc_loglik <- mcmcMat[,grep("^loglik",colnames(mcmcMat))]

loglik <- rowSums(mcmc_loglik)

# sort loglik to determine most likely values
loglik_sorted <- sort(loglik, decreasing = TRUE)



# Prepare Data Simulation ------

simRuns <- 10 # 10 recoveries

# initiate matrix to store simulation results
sim_results <- matrix(nrow = nrow(df_subset), ncol = simRuns)


# write function that determines the index of the parameter
witch <- function(parameter_name){
  which(colnames(mcmcfin_true[[1]]) == parameter_name)
}


# select model file
model_file <- "scripts/04_Modeling/bayes_models/simulation_maaDDM_dirichlet.txt"


# Simulate Data --------

for (sim in 1:simRuns) {
  
  # determine position of simth likely parameter values
  idx <- which(loglik == loglik_sorted[sim])
  
  # initialize matrices for weights
  wT <- matrix(, nrow = SampleSize, ncol = 3)
  wT_AT <- matrix(, nrow = SampleSize, ncol = 3)
  
  # retrieve parameters
  wT[,1] <- unname(unlist(combined_mcmcfin_true[idx, witch("wT[1,1]") : (witch("wT[1,1]") + (SampleSize - 1))]))
  wT[,2] <- unname(unlist(combined_mcmcfin_true[idx, witch("wT[1,2]") : (witch("wT[1,2]") + (SampleSize - 1))]))
  wT[,3] <- unname(unlist(combined_mcmcfin_true[idx, witch("wT[1,3]") : (witch("wT[1,3]") + (SampleSize - 1))]))
  wT_AT[,1] <- unname(unlist(combined_mcmcfin_true[idx, witch("wT_AT[1,1]") : (witch("wT_AT[1,1]") + (SampleSize - 1))]))
  wT_AT[,2] <- unname(unlist(combined_mcmcfin_true[idx, witch("wT_AT[1,2]") : (witch("wT_AT[1,2]") + (SampleSize - 1))]))
  wT_AT[,3] <- unname(unlist(combined_mcmcfin_true[idx, witch("wT_AT[1,3]") : (witch("wT_AT[1,3]") + (SampleSize - 1))]))
  thetaT <- unname(unlist(combined_mcmcfin_true[idx, witch("thetaT[1]") : (witch("thetaT[1]") + (SampleSize - 1))]))
  thetaT_AT <- unname(unlist(combined_mcmcfin_true[idx, witch("thetaT_AT[1]") : (witch("thetaT_AT[1]") + (SampleSize - 1))]))
  phiT <- unname(unlist(combined_mcmcfin_true[idx, witch("phiT[1]") : (witch("phiT[1]") + (SampleSize - 1))]))
  phiT_AT <- unname(unlist(combined_mcmcfin_true[idx, witch("phiT_AT[1]") : (witch("phiT_AT[1]") + (SampleSize - 1))]))
  alpha <- unname(unlist(combined_mcmcfin_true[idx, witch("alpha[1]") : (witch("alpha[1]") + (SampleSize - 1))]))
  alpha_AT <- unname(unlist(combined_mcmcfin_true[idx, witch("alpha_AT[1]") : (witch("alpha_AT[1]") + (SampleSize - 1))]))
  scaling <- unname(unlist(combined_mcmcfin_true[idx, witch("scaling[1]") : (witch("scaling[1]") + (SampleSize - 1))]))
  scaling_AT <- unname(unlist(combined_mcmcfin_true[idx, witch("scaling_AT[1]") : (witch("scaling_AT[1]") + (SampleSize - 1))]))
  tau <- unname(unlist(combined_mcmcfin_true[idx, witch("tau[1]") : (witch("tau[1]") + (SampleSize - 1))]))
  tau_AT <- unname(unlist(combined_mcmcfin_true[idx, witch("tau_AT[1]") : (witch("tau_AT[1]") + (SampleSize - 1))]))
  sp <- unname(unlist(combined_mcmcfin_true[idx, witch("sp[1]") : (witch("sp[1]") + (SampleSize - 1))]))
  sp_AT <- unname(unlist(combined_mcmcfin_true[idx, witch("sp_AT[1]") : (witch("sp_AT[1]") + (SampleSize - 1))]))
  
  
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
                  fixProps_Price_Eco=fixProps$price1,
                  fixProps_Energy_Eco=fixProps$consumption1,
                  fixProps_Popularity_Eco=fixProps$popularity1,
                  fixProps_Price_NonEco=fixProps$price0,
                  fixProps_Energy_NonEco=fixProps$consumption0,
                  fixProps_Popularity_NonEco=fixProps$popularity0,
                  
                  # parameters
                  wT = wT, 
                  wT_AT = wT_AT,
                  thetaT=thetaT,
                  thetaT_AT=thetaT_AT, 
                  phiT=phiT, 
                  phiT_AT=phiT_AT, 
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
filename <- paste0("data/modeling/simResultsmaaDDMDirichlet", "_", "recovery", "_", dataset, "_", translation_of_interest, "_",  time, ".rds")
saveRDS(sim_results, file = filename)



