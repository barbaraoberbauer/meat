#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# purpose: estimate parameters of maaDDM model using bayesian hierarchical estimation
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

load("data/preprocessedDataOriginal.RData")
load("data/preprocessedDataReplication.RData")

### Specify subset of data ----

dataset <- "replication"
# datasets: "original", "replication"

translation_of_interest <- "rating_add"
# translations for original dataset: "control", "emissions", "operating_costs", "environmental_friendliness"
# translations for replication dataset: "control", "emission_add", "rating_add", "emission_replace", "rating_replace"

run_subgroups_separately <- FALSE
# if set to TRUE, estimates parameters separately for participants that did receive an additional price translation at t2 and those who did not
# only applicable to original data

group_of_interest <- "price_translation_present"
# groups: "price_translation_absent", "price_translation_present"
# only applicable to original data


# set data
if (dataset == "original") {
  
  df <- dfOriginal
  
} else if (dataset == "replication") {
  
  df <- dfReplication
    
}

# set subset depending on condition
if (dataset == "original" & run_subgroups_separately == TRUE) {
  
  if (group_of_interest == "price_translation_absent") {
    
    df <- df %>%
      filter(consumption_translation == translation_of_interest & price_translation == 0)
    
  } else if (group_of_interest == "price_translation_present") {
    
    df <- df %>%
      filter(consumption_translation == translation_of_interest & price_translation == 1)
    
  }
  
} else {
  
  df <- df %>%
    filter(consumption_translation == translation_of_interest)
  
}


# Prepare data for modelling ------

### Transform Response Times -----

# Choice response time data should be coded in such a way that lower boundary
# responses are given negative values [1]

# variable indicating response time: t_decision
# choice = selection of energy-efficient option (0 = no, 1 = yes)
# lower boundary will code selection of less energy_efficient option (former 0)

df <- df %>%
  mutate(t_decision = case_when(choice == 0 ~ t_decision * -1/1000,
                                choice == 1 ~ t_decision * 1/1000))

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
  
  # Subject parameters
  "w1",
  "w1T",
  "w2",
  "w2T",
  "w3T",
  "theta",
  "phi",
  "alpha",
  "tau",
  "scaling",
  "sp",
  
  #likelihood
  "loglik"
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
    sigma_sp = sigma_sp
  )
  
}

### Set model specifications ------

nchains <- 6
nAdaptSteps <- 5000
nBurninSteps <- 25000
nUseSteps = nchains * 6500 # total number of used steps
nThinSteps <- 25

### Select model (text file) -----

model_file <- "scripts/04_Modeling/bayes_models/hierarchical_bayesian_maaDDM_single_session.txt"


# Loop across sessions -------

runJagsOutResults <- list()

for (session_of_interest in 1:2) {
  
  # print current state
  print(paste0("current session is ", session_of_interest))
  
  # filter and organize data
  #'-------------------------
  
  # filter session
  df_subset <- df %>%
    filter(session == session_of_interest)
  
  # assign new ids that are starting from 1 and increment by 1
  df_subset <- df_subset %>%
    mutate(id_new = dense_rank(id))
  
  # sort data frame according to id_new (starting from 1 to last participant)
  df_subset <- df_subset[order(df_subset$id_new),]
  
  # sample size
  SampleSize <- length(unique(df_subset$id_new))
  
  # calculate fixation proportions
  #'-------------------------
  
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
  fixProps <- fixProps/df_subset$t_total # divide by total dwell time
  
  # normalize each trial to 1
  fixProps <- fixProps/rowSums(fixProps) 
  
  # specify model
  #'-------------------------

  dat <- list(N=nrow(df_subset),
              x=df_subset$t_decision,
              Subject=df_subset$id_new,
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
  
  # run model
  #'-------------------------
  
  cl <- makePSOCKcluster(nchains)
  # clusterCall(cl, function(x) require("wiener"))
  
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
                         inits = GenInits(),
                         adapt = nAdaptSteps,
                         burnin = nBurninSteps,
                         sample = ceiling(nUseSteps/nchains),
                         thin = nThinSteps,
                         summarise = TRUE,
                         plots = FALSE)
  
  runJagsOutResults[[session_of_interest]] <- runJagsOut
  
}

### Save model output ------

time <- format(Sys.time(), "%Y%m%d_%H%M")

if (dataset == "replication") {
  
  filename <- paste0("data/modeling/runJagsOutSingleSession", "_", dataset, "_", translation_of_interest, "_", time, ".rds")
  
} else if (dataset == "original") {
  
  if (run_subgroups_separately == FALSE) {
    
    filename <- paste0("data/modeling/runJagsOutSingleSession", "_", dataset, "_", translation_of_interest, "_", ".rds")

  } else if (run_subgroups_separately == TRUE) {
    
    filename <- paste0("data/modeling/runJagsOutSingleSession", "_", dataset, "_", translation_of_interest, "_", group_of_interest, "_", time, ".rds")

  }
  
}

saveRDS(runJagsOutResults, file = filename)


# Check results ------

# store as mcmc object
mcmcfin = as.mcmc.list(runJagsOut)

# combine chains
combined_mcmcfin <- as.data.frame(do.call(rbind, mcmcfin))

# set up data frame to store hdis
hdi <- list()

### HDIs weights -------

# see Hellmann, S., Busch, N., Hof, L., & Pachur, T. (2025). Bias by Variance: How Common Parameter Transformations in Hierarchical Models Distort Group-Level Estimates.
# for correct mapping of group-level means onto the parameter scale

###### price -------

map_input_w1 <- combined_mcmcfin$mu_w1/sqrt(1 + combined_mcmcfin$sigma_w1^2)
map_input_dw1 <- combined_mcmcfin$mu_dw1/sqrt(1 + combined_mcmcfin$sigma_dw1)

hdi$w_price <- list(hdi_baseline = HDIofMCMC(pnorm(map_input_w1)),
                    hdi_manipulation = HDIofMCMC(pnorm(map_input_w1 + map_input_dw1)),
                    hdi_change = HDIofMCMC(pnorm(map_input_w1 + map_input_dw1) -
                                             pnorm(map_input_w1)))


###### consumption -------

map_input_w2 <- combined_mcmcfin$mu_w2/sqrt(1 + combined_mcmcfin$sigma_w2^2)
map_input_dw2 <- combined_mcmcfin$mu_dw2/sqrt(1 + combined_mcmcfin$sigma_dw2^2)

hdi$w_consumption <- list(hdi_baseline = HDIofMCMC(pnorm(map_input_w2)),
                          hdi_manipulation = HDIofMCMC(pnorm(map_input_w2 +
                                                               map_input_dw2)),
                          hdi_change = HDIofMCMC(pnorm(map_input_w2 + map_input_dw2) -
                                                   pnorm(map_input_w2)))


###### popularity -------

combined_mcmcfin$mu_w3 <- 1 - pnorm(map_input_w1) - pnorm(map_input_w2)
combined_mcmcfin$mu_w3_AT <- 1 - 
  pnorm(map_input_w1 + map_input_dw1) - 
  pnorm(map_input_w2 + map_input_dw2)

hdi$w_popularity <- list(hdi_baseline = HDIofMCMC(combined_mcmcfin$mu_w3),
                          hdi_manipulation = HDIofMCMC(combined_mcmcfin$mu_w3_AT),
                          hdi_change = HDIofMCMC(combined_mcmcfin$mu_w3_AT - combined_mcmcfin$mu_w3))


### HDIs attentional parameters -------

if (bounded == TRUE) {
  
  map_input_theta <- combined_mcmcfin$mu_theta/sqrt(1 + combined_mcmcfin$sigma_theta^2)
  map_input_dtheta <- combined_mcmcfin$mu_dtheta/sqrt(1 + combined_mcmcfin$sigma_dtheta^2)
  
  hdi$theta <- list(hdi_baseline = HDIofMCMC(pnorm(map_input_theta)),
                            hdi_manipulation = HDIofMCMC(pnorm(map_input_theta + map_input_dtheta)),
                            hdi_change = HDIofMCMC(pnorm(map_input_theta + map_input_dtheta) -
                                                     pnorm(map_input_dtheta)))
  
  map_input_phi <- combined_mcmcfin$mu_phi/sqrt(1 + combined_mcmcfin$sigma_phi^2)
  map_input_dphi <- combined_mcmcfin$mu_dphi/sqrt(1 + combined_mcmcfin$sigma_dphi^2)
  
  hdi$phi <- list(hdi_baseline = HDIofMCMC(pnorm(map_input_phi)),
                    hdi_manipulation = HDIofMCMC(pnorm(map_input_phi + map_input_dphi)),
                    hdi_change = HDIofMCMC(pnorm(map_input_phi + map_input_dphi) -
                                             pnorm(map_input_dphi)))
  
  
} else if (bounded == FALSE) {
  
  hdi$theta <- list(hdi_baseline = HDIofMCMC(combined_mcmcfin$mu_theta),
                    hdi_manipulation = HDIofMCMC(combined_mcmcfin$mu_theta + combined_mcmcfin$mu_dtheta),
                    hdi_change = HDIofMCMC(combined_mcmcfin$mu_dtheta))
  
  hdi$phi <- list(hdi_baseline = HDIofMCMC(combined_mcmcfin$mu_phi),
                    hdi_manipulation = HDIofMCMC(combined_mcmcfin$mu_phi + combined_mcmcfin$mu_dphi),
                    hdi_change = HDIofMCMC(combined_mcmcfin$mu_dphi))
  
}



### HDIs other parameters -------

###### boundary separation ----------

hdi$alpha <- list(hdi_baseline = HDIofMCMC(combined_mcmcfin$mu_alpha),
                hdi_manipulation = HDIofMCMC(combined_mcmcfin$mu_alpha + combined_mcmcfin$mu_dalpha),
                hdi_change = HDIofMCMC(combined_mcmcfin$mu_dalpha))


###### scaling  ----------

hdi$scaling <- list(hdi_baseline = HDIofMCMC(combined_mcmcfin$mu_scaling),
                  hdi_manipulation = HDIofMCMC(combined_mcmcfin$mu_scaling + combined_mcmcfin$mu_dscaling),
                  hdi_change = HDIofMCMC(combined_mcmcfin$mu_dscaling))


###### non-decision time  ----------

hdi$tau <- list(hdi_baseline = HDIofMCMC(combined_mcmcfin$mu_tau),
                    hdi_manipulation = HDIofMCMC(combined_mcmcfin$mu_tau + combined_mcmcfin$mu_dtau),
                    hdi_change = HDIofMCMC(combined_mcmcfin$mu_dtau))


###### starting point bias  ----------

hdi$sp <- list(hdi_baseline = HDIofMCMC(combined_mcmcfin$mu_sp),
                    hdi_manipulation = HDIofMCMC(combined_mcmcfin$mu_sp + combined_mcmcfin$mu_dsp),
                    hdi_change = HDIofMCMC(combined_mcmcfin$mu_dsp))

# Store the results

if (dataset == "replication") {
  
  filename <- paste0("data/modeling/hdi", "_", dataset, "_", translation_of_interest, "_", file_extension, "_", time, ".rds")
  
} else if (dataset == "original") {
  
  if (run_subgroups_separately == FALSE) {
    
    filename <- paste0("data/modeling/hdi", "_", dataset, "_", translation_of_interest, "_", file_extension, "_", time, ".rds")
    
  } else if (run_subgroups_separately == TRUE) {
    
    filename <- paste0("data/modeling/hdi", "_", dataset, "_", translation_of_interest, "_", group_of_interest, "_", file_extension, "_", time, ".rds")
    
  }
  
}

saveRDS(hdi, file = filename)
