#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# last update: "2025-03-07"
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
              "runjags",
              "dplyr",
              "parallel",
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
library(runjags)
library(dplyr)
library(parallel)
library(bayestestR)


# Load functions written by John Kruschke
# https://github.com/boboppie/kruschke-doing_bayesian_data_analysis/blob/master/2e/DBDA2E-utilities.R
#source("functions/DBDA2E-utilities.R")

# Load required modules
load.runjagsmodule("wiener")

rm(package, packages, is_package_installed)


### Load data ------

# specify subset of data 

group_of_interest <- "environmental_friendliness"
# groups: "control", "emissions", "operating_costs", "environmental_friendliness"

file_extension <- "_nobounds" # only supports unbounded parameter estimates

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
fixProps <- fixProps/(df_subset$t_decision/1000)

# normalize each trial to 1
fixProps <- fixProps/rowSums(fixProps)

# Retrieve parameter modes as most likely values  -----

# round parameter estimates to three decimals for modes to have plausible results
combined_mcmcfin <- round(combined_mcmcfin, 3)

### Boundary separation ----

mu_alpha <- as.numeric(names(sort(-table(combined_mcmcfin$mu_alpha)))[1])
sigma_alpha <- as.numeric(names(sort(-table(combined_mcmcfin$sigma_alpha)))[1])

mu_dalpha <- as.numeric(names(sort(-table(combined_mcmcfin$mu_dalpha)))[1])
sigma_dalpha <- as.numeric(names(sort(-table(combined_mcmcfin$sigma_dalpha)))[1])

### Non-decision time -----

mu_tau <- as.numeric(names(sort(-table(combined_mcmcfin$mu_tau)))[1])
sigma_tau <- as.numeric(names(sort(-table(combined_mcmcfin$sigma_tau)))[1])

mu_dtau <- as.numeric(names(sort(-table(combined_mcmcfin$mu_dtau)))[1])
sigma_dtau <- as.numeric(names(sort(-table(combined_mcmcfin$sigma_dtau)))[1])

### Scaling -------

mu_scaling <- as.numeric(names(sort(-table(combined_mcmcfin$mu_scaling)))[1])
sigma_scaling <- as.numeric(names(sort(-table(combined_mcmcfin$sigma_scaling)))[1])

mu_dscaling <- as.numeric(names(sort(-table(combined_mcmcfin$mu_dscaling)))[1])
sigma_dscaling <- as.numeric(names(sort(-table(combined_mcmcfin$sigma_dscaling)))[1])

### Starting point -----

mu_sp <- as.numeric(names(sort(-table(combined_mcmcfin$mu_sp)))[1])
sigma_sp <- as.numeric(names(sort(-table(combined_mcmcfin$sigma_sp)))[1])

mu_dsp <- as.numeric(names(sort(-table(combined_mcmcfin$mu_dsp)))[1])
sigma_dsp <- as.numeric(names(sort(-table(combined_mcmcfin$sigma_dsp)))[1])

### Theta -----

mu_theta <- as.numeric(names(sort(-table(combined_mcmcfin$mu_theta)))[1])
sigma_theta <- as.numeric(names(sort(-table(combined_mcmcfin$sigma_theta)))[1])

mu_dtheta <- as.numeric(names(sort(-table(combined_mcmcfin$mu_dtheta)))[1])
sigma_dtheta <- as.numeric(names(sort(-table(combined_mcmcfin$sigma_dtheta)))[1])

### Phi -----

mu_phi <- as.numeric(names(sort(-table(combined_mcmcfin$mu_phi)))[1])
sigma_phi <- as.numeric(names(sort(-table(combined_mcmcfin$sigma_phi)))[1])

mu_dphi <- as.numeric(names(sort(-table(combined_mcmcfin$mu_dphi)))[1])
sigma_dphi <- as.numeric(names(sort(-table(combined_mcmcfin$sigma_dphi)))[1])

### Weight Price -----

mu_w1 <- as.numeric(names(sort(-table(combined_mcmcfin$mu_w1)))[1])
sigma_w1 <- as.numeric(names(sort(-table(combined_mcmcfin$sigma_w1)))[1])

mu_dw1 <- as.numeric(names(sort(-table(combined_mcmcfin$mu_dw1)))[1])
sigma_dw1 <- as.numeric(names(sort(-table(combined_mcmcfin$sigma_dw1)))[1])

### Weight Consumption -----

mu_w2 <- as.numeric(names(sort(-table(combined_mcmcfin$mu_w2)))[1])
sigma_w2 <- as.numeric(names(sort(-table(combined_mcmcfin$sigma_w2)))[1])

mu_dw2 <- as.numeric(names(sort(-table(combined_mcmcfin$mu_dw2)))[1])
sigma_dw2 <- as.numeric(names(sort(-table(combined_mcmcfin$sigma_dw2)))[1])

# Prepare data simulation ---------

# select model file
model_file <- "04_Modeling/bayes_models/recovery_maaDDM.txt"

# create data list as input for JAGS (this time, parameters serve as input and RT are monitored)
dat_sim <- list(N=nrow(df_subset),
                Subject=df_subset$id_new,
                Session=df_subset$session,
                SampleSize=SampleSize,
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
                mu_alpha=mu_alpha,
                sigma_alpha=sigma_alpha,
                mu_dalpha=mu_dalpha,
                sigma_dalpha=sigma_dalpha,
                mu_tau=mu_tau,
                sigma_tau=sigma_tau,
                mu_dtau=mu_dtau,
                sigma_dtau=sigma_dtau,
                mu_scaling=mu_scaling,
                sigma_scaling=sigma_scaling,
                mu_dscaling=mu_dscaling,
                sigma_dscaling=sigma_dscaling,
                mu_sp=mu_sp,
                sigma_sp=sigma_sp,
                mu_dsp=mu_dsp,
                sigma_dsp=sigma_dsp,
                mu_theta=mu_theta,
                sigma_theta=sigma_theta,
                mu_dtheta=mu_dtheta,
                sigma_dtheta=sigma_dtheta,
                mu_phi=mu_phi,
                sigma_phi=sigma_phi,
                mu_dphi=mu_dphi,
                sigma_dphi=sigma_dphi,
                mu_w1=mu_w1,
                sigma_w1=sigma_w1,
                mu_dw1=mu_dw1,
                sigma_dw1=sigma_dw1,
                mu_w2=mu_w2,
                sigma_w2=sigma_w2,
                mu_dw2=mu_dw2,
                sigma_dw2=sigma_dw2
                
)

# declare variables to be monitored
monitor_sim <- c("x")

# Simulate data ------

results <- run.jags(model = model_file,
                    monitor = monitor_sim,
                    module = "wiener",
                    data = dat_sim,
                    n.chains = 1,
                    sample = 1,
                    silent.jags = TRUE)
