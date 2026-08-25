#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# purpose: estimate parameters of maaDDM model using bayesian hierarchical estimation for subgroups of original study
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
              "bayestestR",
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
library(runjags)
library(dplyr)
library(parallel)
library(bayestestR)
library(truncnorm)


# Load functions written by John Kruschke
# https://github.com/boboppie/kruschke-doing_bayesian_data_analysis/blob/master/2e/DBDA2E-utilities.R
source("R/functions/DBDA2E-utilities.R")

# Load required modules
load.runjagsmodule("wiener")

rm(package, packages, is_package_installed)


### Load data ------

load("data/behavior/preprocessedDataOriginal.RData")

### Specify subset of data ----

translation_of_interest <- "environmental_friendliness"

bound_attention_params <- TRUE # set to true if parameter estimates for theta and phi are supposed to be bound between 0 and 1

dataset <- "original"

df <- dfOriginal

### Model specification ------

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
  "mu_w", 
  "kappa",
  "mu_dalr1",
  "sigma_dalr1",
  "mu_dalr2",
  "sigma_dalr2",
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
  "wT",
  "wT_AT",
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
  "sp_AT",
  
  #likelihood
  "loglik"
  
)

nchains <- 6
nAdaptSteps <- 5000
nBurninSteps <- 25000
nUseSteps = nchains * 6500 # total number of used steps
nThinSteps <- 25

model_file <- "scripts/04_Modeling/bayes_models/hierarchical_bayesian_maaDDM_dirichlet.txt"
  


# Fit parameters ---------

# store in list
allRunJagsOut <- list()

# loop over separate translation subgroups (price translation present or absent)
price_translation_vals <- c(0, 1)
names(price_translation_vals) <- c("price_translation_absent", "price_translation_present")

for (subgroup in 1:2) {
  
  # set subset depending on condition
  df_subset <- df %>%
    filter(consumption_translation == translation_of_interest,
           price_translation == price_translation_vals[subgroup])
  
  # assign new ids that are starting from 1 and increment by 1
  df_subset <- df_subset %>%
    mutate(id_new = dense_rank(id))
  
  # sort data frame according to id_new (starting from 1 to last participant)
  df_subset <- df_subset[order(df_subset$id_new),]
  
  # transform RT
  df_subset <- df_subset %>%
    mutate(t_decision = case_when(choice == 0 ~ t_decision * -1/1000,
                                  choice == 1 ~ t_decision * 1/1000))
  
  # calculate fix props
  # fixProps -> acquistion time for each attribute proportional to the total duration of the trial (vector containing six elements in our case)
  fixProps <- data.frame(price0 = rep(NA, nrow(df_subset)),
                         consumption0 = rep(NA, nrow(df_subset)),
                         popularity0 = rep(NA, nrow(df_subset)),
                         price1 = rep(NA, nrow(df_subset)),
                         consumption1 = rep(NA, nrow(df_subset)),
                         popularity1 = rep(NA, nrow(df_subset))) 
  
  fixProps$price0 <- rowSums(df_subset[, c("t_price0", "t_price_translation0")], na.rm = TRUE)/1000
  fixProps$price1 <- rowSums(df_subset[, c("t_price1", "t_price_translation1")], na.rm = TRUE)/1000
  fixProps$consumption0 <- rowSums(df_subset[, c("t_consumption0", "t_consumption_translation0")], na.rm = TRUE)/1000
  fixProps$popularity0 <- df_subset$t_popularity0/1000
  fixProps$consumption1 <- rowSums(df_subset[, c("t_consumption1", "t_consumption_translation1")], na.rm = TRUE)/1000
  fixProps$popularity1 <- df_subset$t_popularity1/1000
  
  # divide by total duration of the trial
  fixProps <- fixProps/df_subset$t_total # divide by total dwell time
  
  # normalize each trial to 1
  fixProps <- fixProps/rowSums(fixProps) 
  
  # sample size
  SampleSize <- length(unique(df_subset$id_new))
  
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
  
  # Set up initial values
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
    mu_sp = rnorm(1, 0.5, sd)
    sigma_sp = rtruncnorm(1, a = 0, b = Inf, 1, sd)
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
    
    # group weights — random but close to equal, never near 0
    w_init <- as.vector(MCMCpack::rdirichlet(1, c(10, 10, 10)))
    
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
      mu_sp = mu_sp,
      sigma_sp = sigma_sp,
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
      sigma_dsp = sigma_dsp,
      
      # Dirichlet weights — randomized across chains, kept near equal
      mu_w     = w_init,
      
      # Kappa — fixed start
      kappa    = 5,
      
      # ALR change parameters — start at no change
      mu_dalr1 = 0,
      mu_dalr2 = 0
    )
    
  }
  
  # Run model 
  
  # set up cluster manually and make sure module is loaded before running the model
  cl <- makePSOCKcluster(nchains)

  clusterEvalQ(cl, {
    library(rjags)
    library(runjags)
    load.runjagsmodule("wiener")  
  })
  
  runJagsOut <- run.jags(method = "parallel",
                         model = model_file,
                         monitor = monitor,
                         module = "wiener",
                         data = dat,
                         n.chains = nchains,
                         #inits = GenInits(),
                         adapt = nAdaptSteps,
                         burnin = nBurninSteps,
                         sample = ceiling(nUseSteps/nchains),
                         thin = nThinSteps,
                         summarise = TRUE,
                         plots = FALSE)
  
  # Save model output 
  
  time <- format(Sys.time(), "%Y%m%d_%H%M")
  
  filename <- paste0("data/modeling/runJagsOutmaaDDMDirichlet", "_", 
                     dataset, "_",
                     translation_of_interest, "_", 
                     names(price_translation)[subgroup], "_",
                     time, ".rds")
    
  saveRDS(runJagsOut, file = filename)
  
  allRunJagsOut[names(price_translation)[subgroup]] <- runJagsOut
  
  closeAllConnections()
  
}

# Check results ------

# store as mcmc object
mcmcfin_absent = as.mcmc.list(allRunJagsOut$price_translation_absent)
mcmcfin_present = as.mcmc.list(allRunJagsOut$price_translation_present)

# combine chains
combined_mcmcfin_absent <- as.data.frame(do.call(rbind, mcmcfin_absent))
combined_mcmcfin_present <- as.data.frame(do.call(rbind, mcmcfin_present))

# set up data frame to store hdis
hdi <- list()

# ### HDIs weights -------

# Function to do the forward-transform, add change, back-transform
compute_mu_w_AT <- function(mcmc_obj) {
  
  mat <- as.matrix(mcmc_obj)
  
  mu_w_samples     <- mat[, c("mu_w[1]", "mu_w[2]", "mu_w[3]")]
  mu_dalr1_samples <- mat[, "mu_dalr1"]
  mu_dalr2_samples <- mat[, "mu_dalr2"]
  
  # Forward transform group mean Session 1 weights to ALR space
  mu_alr1 <- log(mu_w_samples[,1] / mu_w_samples[,3])
  mu_alr2 <- log(mu_w_samples[,2] / mu_w_samples[,3])
  
  # Add group mean change
  mu_alr1_AT <- mu_alr1 + mu_dalr1_samples
  mu_alr2_AT <- mu_alr2 + mu_dalr2_samples
  
  # Back-transform to simplex
  exp1_AT  <- exp(mu_alr1_AT)
  exp2_AT  <- exp(mu_alr2_AT)
  denom_AT <- 1 + exp1_AT + exp2_AT
  
  list(
    mu_w_AT_1 = exp1_AT / denom_AT,  # price weight, Session 2
    mu_w_AT_2 = exp2_AT / denom_AT,  # energy weight, Session 2
    mu_w_AT_3 = 1       / denom_AT   # popularity weight, Session 2
  )
}

# apply function to both mcmcfin objects
weights_absent <- compute_mu_w_AT(combined_mcmcfin_absent)
weights_present <- compute_mu_w_AT(combined_mcmcfin_present)


# Save in HDIs

hdi$w_price <- list(hdi_absent_change = HDIofMCMC(unname(weights_absent$mu_w_AT_1) -
                                             combined_mcmcfin_absent$`mu_w[1]`),
                    hdi_present_change = HDIofMCMC(unname(weights_present$mu_w_AT_1) -
                                              combined_mcmcfin_present$`mu_w[1]`),
                    hdi_diff = HDIofMCMC((unname(weights_present$mu_w_AT_1) -
                                            combined_mcmcfin_present$`mu_w[1]`) - 
                                           (unname(weights_absent$mu_w_AT_1) -
                                              combined_mcmcfin_absent$`mu_w[1]`))
)

hdi$w_consumption <- list(hdi_absent_change = HDIofMCMC(unname(weights_absent$mu_w_AT_2) -
                                                    combined_mcmcfin_absent$`mu_w[2]`),
                    hdi_present_change = HDIofMCMC(unname(weights_present$mu_w_AT_2) -
                                                     combined_mcmcfin_present$`mu_w[2]`),
                    hdi_diff = HDIofMCMC((unname(weights_present$mu_w_AT_2) -
                                            combined_mcmcfin_present$`mu_w[2]`) - 
                                           (unname(weights_absent$mu_w_AT_2) -
                                              combined_mcmcfin_absent$`mu_w[2]`))
)

hdi$w_popularity <- list(hdi_absent_change = HDIofMCMC(unname(weights_absent$mu_w_AT_3) -
                                                          combined_mcmcfin_absent$`mu_w[3]`),
                          hdi_present_change = HDIofMCMC(unname(weights_present$mu_w_AT_3) -
                                                           combined_mcmcfin_present$`mu_w[3]`),
                          hdi_diff = HDIofMCMC((unname(weights_present$mu_w_AT_3) -
                                                  combined_mcmcfin_present$`mu_w[3]`) - 
                                                 (unname(weights_absent$mu_w_AT_3) -
                                                    combined_mcmcfin_absent$`mu_w[3]`))
)


### HDIs attentional parameters -------


hdi$theta <- list(hdi_absent_change = HDIofMCMC(combined_mcmcfin_absent$mu_dtheta),
                  hdi_present_change = HDIofMCMC(combined_mcmcfin_present$mu_dtheta),
                  hdi_diff = HDIofMCMC(combined_mcmcfin_present$mu_dtheta -
                                         combined_mcmcfin_absent$mu_dtheta))

hdi$phi <- list(hdi_absent_change = HDIofMCMC(combined_mcmcfin_absent$mu_dphi),
                hdi_present_change = HDIofMCMC(combined_mcmcfin_present$mu_dphi),
                hdi_diff = HDIofMCMC(combined_mcmcfin_present$mu_dphi -
                                       combined_mcmcfin_absent$mu_dphi))


### HDIs other parameters -------

###### boundary separation ----------

hdi$alpha <- list(hdi_absent_change = HDIofMCMC(combined_mcmcfin_absent$mu_dalpha),
                  hdi_present_change = HDIofMCMC(combined_mcmcfin_present$mu_dalpha),
                  hdi_diff = HDIofMCMC(combined_mcmcfin_present$mu_dalpha -
                                         combined_mcmcfin_absent$mu_dalpha))

###### scaling  ----------

hdi$scaling <- list(hdi_absent_change = HDIofMCMC(combined_mcmcfin_absent$mu_dscaling),
                    hdi_present_change = HDIofMCMC(combined_mcmcfin_present$mu_dscaling),
                    hdi_diff = HDIofMCMC(combined_mcmcfin_present$mu_dscaling -
                                           combined_mcmcfin_absent$mu_dscaling))


###### non-decision time  ----------

hdi$tau <- list(hdi_absent_change = HDIofMCMC(combined_mcmcfin_absent$mu_dtau),
                hdi_present_change = HDIofMCMC(combined_mcmcfin_present$mu_dtau),
                hdi_diff = HDIofMCMC(combined_mcmcfin_present$mu_dtau -
                                       combined_mcmcfin_absent$mu_dtau))


###### starting point bias  ----------

hdi$sp <- list(hdi_absent_change = HDIofMCMC(combined_mcmcfin_absent$mu_dsp),
               hdi_present_change = HDIofMCMC(combined_mcmcfin_present$mu_dsp),
               hdi_diff = HDIofMCMC(combined_mcmcfin_present$mu_dsp -
                                      combined_mcmcfin_absent$mu_dsp))


filename <- paste0("data/modeling/hdimaaDDMDirichlet", "_", 
                     dataset, "_",
                     translation_of_interest, "_",
                     "price_translation_subgroups", "_",
                     time, ".rds")

saveRDS(hdi, file = filename)
