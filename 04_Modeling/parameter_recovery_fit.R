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
              "parallel",
              "rjags",
              "runjags",
              "truncnorm")

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
library(truncnorm)

# Load required modules
load.module("wiener")

# Load functions written by John Kruschke
# https://github.com/boboppie/kruschke-doing_bayesian_data_analysis/blob/master/2e/DBDA2E-utilities.R
source("functions/DBDA2E-utilities.R")

rm(package, packages, is_package_installed)


### Load data ------

# specify subset of data 

group_of_interest <- "environmental_friendliness"
# groups: "control", "emissions", "operating_costs", "environmental_friendliness"

# only supports unbounded model 

file_extension <- "_nobounds"

filename <- paste0("data/simResults_", group_of_interest, file_extension, "_recovery", ".rds")

sim_results <- readRDS(filename)

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


# Estimate Parameters for Simulated Data ------

# recover parameters for the data set simulated using the parameter set with rank likelihood (1-10)
rank_likelihood <- 8 # 1 = most likely parameter set was used for simulating the data

### Put data in list ------

# put data in a list for simple use in the run.jags() command
# this was recommended in the example code of dwiener 

dat <- list(N=nrow(df_subset),
            x=sim_results[,rank_likelihood],
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
  "sp_AT"
)

### Set up initial values ------

sd <- 0.1

GenInits = function() {
  
  mu_alpha = rnorm(1, 6, sd)
  sigma_alpha = rtruncnorm(1, a = 0, b = Inf, 1, sd)
  mu_tau = rnorm(1, 0.5, sd)
  sigma_tau = rtruncnorm(1, a = 0, b = Inf, 1, sd)
  mu_scaling = rnorm(1, 1, sd)
  sigma_scaling = rtruncnorm(1, a = 0, b = Inf, 1, sd)
  mu_theta = rnorm(1, 0.5, sd)
  sigma_theta = rtruncnorm(1, a = 0, b = Inf, 1, sd)
  mu_phi = rnorm(1, 0.5, sd)
  sigma_phi = rtruncnorm(1, a = 0, b = Inf, 1, sd)
  mu_w1 = rnorm(1, 0, sd)
  sigma_w1 = rtruncnorm(1, a = 0, b = Inf, 1, sd)
  mu_w2 = rnorm(1, 0, sd)
  sigma_w2 = rtruncnorm(1, a = 0, b = Inf, 1, sd)
  mu_sp = rnorm(1, 0.5, sd)
  sigma_sp = rtruncnorm(1, a = 0, b = Inf, 1, sd)
  mu_dw1 = rnorm(1, 0, sd)
  sigma_dw1 = rtruncnorm(1, a = 0, b = Inf, 1, sd)
  mu_dw2 = rnorm(1, 0, sd)
  sigma_dw2 = rtruncnorm(1, a = 0, b = Inf, 1, sd)
  mu_dtheta = rnorm(1, 0, sd)
  sigma_dtheta = rtruncnorm(1, a = 0, b = Inf, 1, sd)
  mu_dphi = rnorm(1, 0, sd)
  sigma_dphi = rtruncnorm(1, a = 0, b = Inf, 1, sd)
  mu_dalpha = rnorm(1, 0, sd)
  sigma_dalpha = rtruncnorm(1, a = 0, b = Inf, 1, sd)
  mu_dscaling = rnorm(1, 0, sd)
  sigma_dscaling = rtruncnorm(1, a = 0, b = Inf, 1, sd)
  mu_dtau = rnorm(1, 0, sd)
  sigma_dtau = rtruncnorm(1, a = 0, b = Inf, 1, sd)
  mu_dsp = rnorm(1, 0, sd)
  sigma_dsp = rtruncnorm(1, a = 0, b = Inf, 1, sd)
  
  list(
    mu_alpha = mu_alpha,
    sigma_alpha = sigma_alpha,
    mu_tau = mu_tau,
    sigma_tau = sigma_tau,
    mu_scaling = mu_scaling,
    sigma_scaling = sigma_scaling,
    mu_theta = mu_theta,
    sigma_theta = sigma_theta,
    mu_phi = mu_phi,
    sigma_phi = sigma_phi,
    mu_w1 = mu_w1,
    sigma_w1 = sigma_w1,
    mu_w2 = mu_w2,
    sigma_w2 = sigma_w2,
    mu_sp = mu_sp,
    sigma_sp = sigma_sp,
    mu_dw1 = mu_dw1,
    sigma_dw1 = sigma_dw1,
    mu_dw2 = mu_dw2,
    sigma_dw2 = sigma_dw2,
    mu_dtheta = mu_dtheta,
    sigma_dtheta = sigma_dtheta,
    mu_dphi = mu_dphi,
    sigma_dphi = sigma_dphi,
    mu_dalpha = mu_dalpha,
    sigma_dalpha = sigma_dalpha,
    mu_dscaling = mu_dscaling,
    sigma_dscaling = sigma_dscaling,
    mu_dtau = mu_dtau,
    sigma_dtau = sigma_dtau,
    mu_dsp = mu_dsp,
    sigma_dsp = sigma_dsp
  )
  
}

### Set model specifications ------

nchains <- 6
nAdaptSteps <- 5000
nBurninSteps <- 25000
nUseSteps = nchains * 6500 # total number of used steps
nThinSteps <- 25

### Select model (text file) -----

model_file <- "04_Modeling/bayes_models/hierarchical_bayesian_maaDDM_nobounds.txt"

# Run model ------

# set up cluster manually and make sure module is loaded before running the model
# https://sourceforge.net/p/runjags/forum/general/thread/e34ce49c3c/ 
cl <- makePSOCKcluster(nchains)
# clusterCall(cl, function(x) require("wiener"))

clusterEvalQ(cl, {
  library(rjags)
  library(runjags)
  load.runjagsmodule("wiener")  
})

runJagsOut_recovery <- run.jags(method = "parallel",
                                model = model_file,
                                monitor = monitor,
                                module = "wiener",
                                data = dat,
                                n.chains = nchains,
                                inits = GenInits(),
                                adapt = nAdaptSteps,
                                burnin = nBurninSteps,
                                sample = ceiling(nUseSteps/nchains),
                                thin = nThinSteps,
                                summarise = TRUE,
                                plots = FALSE)

### Save model output ------

filename <- paste0("data/runJagsOut_recovery", rank_likelihood, "_", group_of_interest, file_extension, ".rds")
saveRDS(runJagsOut_recovery, file = filename)




# ### Summary statistics -----
# 
# summary_statistics <- add.summary(runJagsOut_recovery,
#                                   vars = c("mu_alpha",
#                                            "mu_dalpha",
#                                            "mu_scaling",
#                                            "mu_dscaling",
#                                            "mu_tau",
#                                            "mu_dtau",
#                                            "mu_theta",
#                                            "mu_dtheta",
#                                            "mu_phi",
#                                            "mu_dphi",
#                                            "mu_w1",
#                                            "mu_dw1",
#                                            "mu_w2",
#                                            "mu_dw2",
#                                            "mu_sp",
#                                            "mu_dsp"))
# 
# ### retrieve summary for phi-transformed weights
# 
# # store as mcmc object
# mcmcfin_recovery = as.mcmc.list(runJagsOut_recovery)
# 
# # combine chains
# combined_mcmcfin_recovery <- as.data.frame(do.call(rbind, mcmcfin_recovery))
# 
# ###### price 
# 
# w_price <- list(hdi_baseline = HDIofMCMC(pnorm(combined_mcmcfin_recovery$mu_w1)),
#                 hdi_manipulation = HDIofMCMC(pnorm(combined_mcmcfin_recovery$mu_w1 + combined_mcmcfin_recovery$mu_dw1)),
#                 hdi_change = HDIofMCMC(pnorm(combined_mcmcfin_recovery$mu_w1 + combined_mcmcfin_recovery$mu_dw1) -
#                                          pnorm(combined_mcmcfin_recovery$mu_w1)))
# 
# 
# ###### consumption 
# 
# w_consumption <- list(hdi_baseline = HDIofMCMC(pnorm(combined_mcmcfin_recovery$mu_w2)),
#                       hdi_manipulation = HDIofMCMC(pnorm(combined_mcmcfin_recovery$mu_w2 + combined_mcmcfin_recovery$mu_dw2)),
#                       hdi_change = HDIofMCMC(pnorm(combined_mcmcfin_recovery$mu_w2 + combined_mcmcfin_recovery$mu_dw2) -
#                                                pnorm(combined_mcmcfin_recovery$mu_w2)))
