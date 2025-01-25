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
              "runjags",
              "dplyr",
              "parallel")

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

# Load required modules
load.runjagsmodule("wiener")

# Load required functions
source("functions/fun_maaDDM.R")


rm(package, packages, is_package_installed)


### Load data ------

df <- readRDS("data/df.rds")

### Specify subset of data ----

group_of_interest <- "environmental_friendliness"
# groups: "control", "emissions", "operating_costs", "environmental_friendliness"

df_subset <- df %>%
  filter(consumption_translation == group_of_interest)

# assign new ids that are starting from 1 and increment by 1
df_subset <- df_subset %>%
  mutate(id_new = dense_rank(id))

# sort data frame according to id_new (starting from 1 to last participant)
df_subset <- df_subset[order(df_subset$id_new),]

# create data frame for minRT
min_rt <- df_subset %>%
  group_by(id_new, session) %>%
  summarize(minRT = min(t_decision)/1000)


# Prepare data for modelling ------

### Transform Response Times -----

# Choice response time data should be coded in such a way that lower boundary
# responses are given negative values [1]

# variable indicating response time: t_decision
# choice = selection of energy-efficient option (0 = no, 1 = yes)
# lower boundary will code selection of less energy_efficient option (former 0)

df_subset <- df_subset %>%
  mutate(t_decision = case_when(choice == 0 ~ t_decision * -1/1000,
                                choice == 1 ~ t_decision * 1/1000))


### Calculate fixation proportions (fixprops) ------

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
fixProps <- fixProps/abs(df$t_decision) #take absolute value instead of +/- coded RT

# normalize each trial to 1
fixProps <- fixProps/rowSums(fixProps) 

# sample size
SampleSize <- length(unique(df_subset$id_new))

# Specify model ------

### Put data in list ------

# put data in a list for simple use in the run.jags() command
# this was recommended in the example code of dwiener 

dat <- list(N=nrow(df_subset),
            x=df_subset$t_decision,
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
            fixProps_Popularity_NonEco=fixProps$popularity0)


### Declare variables to be monitored ------

monitor <- c(
  # Group priors
  "mu_alpha",
  "sigma_alpha",
  "mu_tau",
  "sigma_tau",
  "mu_scaling",
  "sigma_scaling",
  "mu_theta",
  "sigma_theta",
  "mu_phi",
  "sigma_phi",
  "mu_phi_sus",
  "sigma_phi_sus",
  "mu_w1",
  "sigma_w1",
  "mu_w2",
  "sigma_w2",
  "mu_dw1",
  "sigma_dw1",
  "mu_dw2",
  "sigma_dw2",
  "mu_dtheta",
  "sigma_dtheta",
  "mu_dphi",
  "sigma_dphi",
  "mu_dphi_sus",
  "sigma_dphi_sus",
  "mu_dalpha",
  "sigma_dalpha",
  "mu_dscaling",
  "sigma_dscaling",
  "mu_dtau",
  "sigma_dtau",
  "mu_sp",
  "sigma_sp",
  "mu_dsp",
  "sigma_dsp",
  
  # Subject parameters
  "w1",
  "w1T",
  "dw1",
  "w1T_AT",
  "w2",
  "w2T",
  "dw2",
  "w3T",
  "w3T_AT",
  "w2T_AT",
  "theta",
  "thetaT",
  "dtheta",
  "thetaT_AT",
  "phi",
  "phiT",
  "dphi",
  "phiT_AT",
  "phi_sus",
  "phiT_sus",
  "dphi_sus",
  "phiT_AT_sus",
  "alpha",
  "dalpha",
  "alpha_AT",
  "tau",
  "dtau",
  "tau_AT",
  "scaling",
  "dscaling",
  "scaling_AT",
  "sp",
  "dsp",
  "sp_AT",
  
  #likelihood
  "loglik"
  
)

### Set model specifications ------

nchains <- 6
nAdaptSteps <- 5000
nBurninSteps <- 25000
nThinSteps <- 22

model_specifications <- list(
  nchains = nchains,
  nAdaptSteps = nAdaptSteps,
  nBurninSteps = nBurninSteps,
  nUseSteps = nchains * 5000, # total number of used steps
  nThinSteps = nThinSteps
)

### Select model (text file) -----

model_file <- "04_Modeling/bayes_models/hierarchical_bayesian_maaDDM_nobounds.txt"


# Run model ------

runJagsOut <- maaDDM(dat, monitor, model_file, model_specifications)

### Save model output ------

filename <- paste0("data/runJagsOut_", group_of_interest, ".rds")
saveRDS(runJagsOut, file = filename)
