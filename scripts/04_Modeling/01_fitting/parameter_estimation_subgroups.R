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

dataset <- "original"
# datasets: "original", "replication"

translation_of_interest <- "environmental_friendliness"
# translations for original dataset: "control", "emissions", "operating_costs", "environmental_friendliness"
# translations for replication dataset: "control", "emission_add", "rating_add", "emission_replace", "rating_replace"

run_subgroups_separately <- FALSE
# if set to TRUE, estimates parameters separately for participants that did receive an additional price translation at t2 and those who did not
# only applicable to original data

group_of_interest <- "price_translation_present"
# groups: "price_translation_absent", "price_translation_present"
# only applicable to original data

bounded <- FALSE # set to TRUE for bounded model, set to FALSE for unbounded model (bounded vs. unbounded attentional parameters)

# set data

if (dataset == "original") {
  
  df <- dfOriginal
  
} else if (dataset == "replication") {
  
  df <- dfReplication
    
}

# set subset depending on condition

if (dataset == "original" & run_subgroups_separately == TRUE) {
  
  if (group_of_interest == "price_translation_absent") {
    
    df_subset <- df %>%
      filter(consumption_translation == translation_of_interest & price_translation == 0)
    
  } else if (group_of_interest == "price_translation_present") {
    
    df_subset <- df %>%
      filter(consumption_translation == translation_of_interest & price_translation == 1)
    
  }
  
} else {
  
  df_subset <- df %>%
    filter(consumption_translation == translation_of_interest)
  
}


# Perform a median split ---------

# Split participants into groups based on their probability to choose the more ecological option in session 1

# calculate probability to choose eco option in session 1
PChooseEco <- df_subset %>%
  filter(session == 1) %>%
  group_by(id) %>%
  summarize(PEcoChoiceSession1 = mean(choice))

df_subset <- df_subset %>%
  left_join(PChooseEco,
            by = "id")

medianEcoChoice <- median(df_subset$PEcoChoiceSession1)

# remove participants whose probability to choose the more ecological appliance is equal to the median
df_subset <- df_subset %>%
  filter(PEcoChoiceSession1 != medianEcoChoice)

df_subset$HighPEcoChoice <- NA

df_subset$HighPEcoChoice[df_subset$PEcoChoiceSession1 <= medianEcoChoice] <- 0
df_subset$HighPEcoChoice[df_subset$PEcoChoiceSession1 > medianEcoChoice] <- 1

dfHighEcoChoice <- df_subset %>%
  filter(df_subset$HighPEcoChoice == 1)

# assign new ids that are starting from 1 and increment by 1
dfHighEcoChoice <- dfHighEcoChoice %>%
  mutate(id_new = dense_rank(id))

# sort data frame according to id_new (starting from 1 to last participant)
dfHighEcoChoice <- dfHighEcoChoice[order(dfHighEcoChoice$id_new),]

dfLowEcoChoice <- df_subset %>%
  filter(df_subset$HighPEcoChoice == 0)

# assign new ids that are starting from 1 and increment by 1
dfLowEcoChoice <- dfLowEcoChoice %>%
  mutate(id_new = dense_rank(id))

# sort data frame according to id_new (starting from 1 to last participant)
dfLowEcoChoice <- dfLowEcoChoice[order(dfLowEcoChoice$id_new),]


# Prepare data for modelling ------

prepData <- function(data){
  
  ### Transform Response Times 
  
  # Choice response time data should be coded in such a way that lower boundary
  # responses are given negative values [1]
  
  # variable indicating response time: t_decision
  # choice = selection of energy-efficient option (0 = no, 1 = yes)
  # lower boundary will code selection of less energy_efficient option (former 0)
  
  data <- data %>%
    mutate(t_decision = case_when(choice == 0 ~ t_decision * -1/1000,
                                  choice == 1 ~ t_decision * 1/1000))
  
  
  ### Calculate fixation proportions (fixprops) 
  
  # fixProps -> acquistion time for each attribute proportional to the total duration of the trial (vector containing six elements in our case)
  fixProps <- data.frame(price0 = rep(NA, nrow(data)),
                         consumption0 = rep(NA, nrow(data)),
                         popularity0 = rep(NA, nrow(data)),
                         price1 = rep(NA, nrow(data)),
                         consumption1 = rep(NA, nrow(data)),
                         popularity1 = rep(NA, nrow(data))) 
  
  # attributes and their translation are treated as one attribute for simplicity
  # depending on dataset, summarize price and price translation
  if (dataset == "original") {
    
    fixProps$price0 <- rowSums(data[, c("t_price0", "t_price_translation0")], na.rm = TRUE)/1000
    fixProps$price1 <- rowSums(data[, c("t_price1", "t_price_translation1")], na.rm = TRUE)/1000
    
  } else if (dataset == "replication") {
    
    fixProps$price0 <- data$t_price0/1000
    fixProps$price1 <- data$t_price1/1000
    
  }
  
  fixProps$consumption0 <- rowSums(data[, c("t_consumption0", "t_consumption_translation0")], na.rm = TRUE)/1000
  fixProps$popularity0 <- data$t_popularity0/1000
  fixProps$consumption1 <- rowSums(data[, c("t_consumption1", "t_consumption_translation1")], na.rm = TRUE)/1000
  fixProps$popularity1 <- data$t_popularity1/1000
  
  # divide by total dwell time
  fixProps <- fixProps/data$t_total 
  
  # normalize each trial to 1
  fixProps <- fixProps/rowSums(fixProps)
  
  # sample size
  SampleSize <- length(unique(data$id_new))
  
  # return inputs
  modelInput <- list(data = data,
                     fixProps = fixProps,
                     SampleSize = SampleSize)
  
  return(modelInput)
  
}

modelInputHighEcoChoice <- prepData(dfHighEcoChoice)
modelInputLowEcoChoice <- prepData(dfLowEcoChoice)


# Specify model ------

specifyModel <- function(data, SampleSize, fixProps){
  
  ### Put data in list 
  
  # put data in a list for simple use in the run.jags() command
  # this was recommended in the example code of dwiener 
  
  dat <- list(N=nrow(data),
              x=data$t_decision,
              Subject=data$id_new,
              Session=data$session,
              SampleSize=SampleSize,
              Price_Eco=data$priceEco,
              Energy_Eco=data$energyEco,
              Popularity_Eco=data$popularityEco,
              Price_NonEco=data$priceNonEco,
              Energy_NonEco=data$energyNonEco,
              Popularity_NonEco=data$popularityNonEco,
              fixProps_Price_Eco=fixProps$price1,
              fixProps_Energy_Eco=fixProps$consumption1,
              fixProps_Popularity_Eco=fixProps$popularity1,
              fixProps_Price_NonEco=fixProps$price0,
              fixProps_Energy_NonEco=fixProps$consumption0,
              fixProps_Popularity_NonEco=fixProps$popularity0)
  
  return(dat)
  
}

datHighEcoChoice <- specifyModel(modelInputHighEcoChoice$data, 
                                 modelInputHighEcoChoice$SampleSize, 
                                 modelInputHighEcoChoice$fixProps)

datLowEcoChoice <- specifyModel(modelInputLowEcoChoice$data, 
                                 modelInputLowEcoChoice$SampleSize, 
                                 modelInputLowEcoChoice$fixProps)


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
  "sp_AT",
  
  #likelihood
  "loglik"
  
)

### Set up initial values ------

sd <- 0.1

GenInits = function() {

  mu_alpha = rnorm(1, 6, sd)
  sigma_alpha = rtruncnorm(1, a = 0, b = Inf, 1, sd)
  mu_tau = rnorm(1, 0.3, sd)
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

if (bounded == TRUE) {
 
  model_file <- "scripts/04_Modeling/bayes_models/hierarchical_bayesian_maaDDM_bounds.txt"
  file_extension <- "bounds"
  
} else if (bounded == FALSE) {
  
  model_file <- "scripts/04_Modeling/bayes_models/hierarchical_bayesian_maaDDM_nobounds.txt"
  file_extension <- "nobounds"
  
}


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


runParameterEstimation <- function(dat,
                                  model_file,
                                  monitor,
                                  nchains,
                                  GenInits,
                                  nAdaptSteps,
                                  nBurninSteps,
                                  nUseSteps,
                                  nThinSteps){

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

 return(runJagsOut)

}

runJagsOutLowEcoChoice <- runParameterEstimation(datLowEcoChoice,
                                                  model_file,
                                                  monitor,
                                                  nchains,
                                                  GenInits,
                                                  nAdaptSteps,
                                                  nBurninSteps,
                                                  nUseSteps,
                                                  nThinSteps)


runJagsOutHighEcoChoice <- runParameterEstimation(datHighEcoChoice,
                                                 model_file,
                                                 monitor,
                                                 nchains,
                                                 GenInits,
                                                 nAdaptSteps,
                                                 nBurninSteps,
                                                 nUseSteps,
                                                 nThinSteps)



### Save model output ------

time <- format(Sys.time(), "%Y%m%d_%H%M")

if (dataset == "replication") {
  
  filename <- paste0("data/modeling/runJagsOut", "_", dataset, "_", translation_of_interest, "_", file_extension, "_", "EcoSubgroups", "_", time, ".rds")

} else if (dataset == "original") {
  
  if (run_subgroups_separately == FALSE) {
    
    filename <- paste0("data/modeling/runJagsOut", "_", dataset, "_", translation_of_interest, "_", file_extension, "_", "EcoSubgroups", "_", time, ".rds")

  } else if (run_subgroups_separately == TRUE) {
    
    filename <- paste0("data/modeling/runJagsOut", "_", dataset, "_", translation_of_interest, "_", group_of_interest, "_", file_extension, "_", "EcoSubgroups", "_", time, ".rds")

  }
  
}

save(runJagsOutHighEcoChoice, runJagsOutLowEcoChoice, file = filename)


# Check results ------

getHDIs <- function(runJagsOut){
  
  # store as mcmc object
  mcmcfin = as.mcmc.list(runJagsOut)
  
  # combine chains
  combined_mcmcfin <- as.data.frame(do.call(rbind, mcmcfin))
  
  # set up data frame to store hdis
  hdi <- list()
  
  ### HDIs weights 
  
  # see Hellmann, S., Busch, N., Hof, L., & Pachur, T. (2025). Bias by Variance: How Common Parameter Transformations in Hierarchical Models Distort Group-Level Estimates.
  # for correct mapping of group-level means onto the parameter scale
  
  ###### price 
  
  map_input_w1 <- combined_mcmcfin$mu_w1/sqrt(1 + combined_mcmcfin$sigma_w1^2)
  map_input_dw1 <- combined_mcmcfin$mu_dw1/sqrt(1 + combined_mcmcfin$sigma_dw1)
  
  hdi$w_price <- list(hdi_baseline = HDIofMCMC(pnorm(map_input_w1)),
                      hdi_manipulation = HDIofMCMC(pnorm(map_input_w1 + map_input_dw1)),
                      hdi_change = HDIofMCMC(pnorm(map_input_w1 + map_input_dw1) -
                                               pnorm(map_input_w1)))
  
  
  ###### consumption 
  
  map_input_w2 <- combined_mcmcfin$mu_w2/sqrt(1 + combined_mcmcfin$sigma_w2^2)
  map_input_dw2 <- combined_mcmcfin$mu_dw2/sqrt(1 + combined_mcmcfin$sigma_dw2^2)
  
  hdi$w_consumption <- list(hdi_baseline = HDIofMCMC(pnorm(map_input_w2)),
                            hdi_manipulation = HDIofMCMC(pnorm(map_input_w2 +
                                                                 map_input_dw2)),
                            hdi_change = HDIofMCMC(pnorm(map_input_w2 + map_input_dw2) -
                                                     pnorm(map_input_w2)))
  
  
  ###### popularity 
  
  combined_mcmcfin$mu_w3 <- 1 - pnorm(map_input_w1) - pnorm(map_input_w2)
  combined_mcmcfin$mu_w3_AT <- 1 - 
    pnorm(map_input_w1 + map_input_dw1) - 
    pnorm(map_input_w2 + map_input_dw2)
  
  hdi$w_popularity <- list(hdi_baseline = HDIofMCMC(combined_mcmcfin$mu_w3),
                           hdi_manipulation = HDIofMCMC(combined_mcmcfin$mu_w3_AT),
                           hdi_change = HDIofMCMC(combined_mcmcfin$mu_w3_AT - combined_mcmcfin$mu_w3))
  
  
  ### HDIs attentional parameters 
  
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
  
  
  
  ### HDIs other parameters 
  
  ###### boundary separation 
  
  hdi$alpha <- list(hdi_baseline = HDIofMCMC(combined_mcmcfin$mu_alpha),
                    hdi_manipulation = HDIofMCMC(combined_mcmcfin$mu_alpha + combined_mcmcfin$mu_dalpha),
                    hdi_change = HDIofMCMC(combined_mcmcfin$mu_dalpha))
  
  
  ###### scaling  
  
  hdi$scaling <- list(hdi_baseline = HDIofMCMC(combined_mcmcfin$mu_scaling),
                      hdi_manipulation = HDIofMCMC(combined_mcmcfin$mu_scaling + combined_mcmcfin$mu_dscaling),
                      hdi_change = HDIofMCMC(combined_mcmcfin$mu_dscaling))
  
  
  ###### non-decision time  
  
  hdi$tau <- list(hdi_baseline = HDIofMCMC(combined_mcmcfin$mu_tau),
                  hdi_manipulation = HDIofMCMC(combined_mcmcfin$mu_tau + combined_mcmcfin$mu_dtau),
                  hdi_change = HDIofMCMC(combined_mcmcfin$mu_dtau))
  
  
  ###### starting point bias  
  
  hdi$sp <- list(hdi_baseline = HDIofMCMC(combined_mcmcfin$mu_sp),
                 hdi_manipulation = HDIofMCMC(combined_mcmcfin$mu_sp + combined_mcmcfin$mu_dsp),
                 hdi_change = HDIofMCMC(combined_mcmcfin$mu_dsp))
  
  return(hdi)
  
}

hdiHighEcoChoice <- getHDIs(runJagsOutHighEcoChoice)
hdiLowEcoChoice <- getHDIs(runJagsOutLowEcoChoice)


# Store the results

if (dataset == "replication") {
  
  filename <- paste0("data/modeling/hdi", "_", dataset, "_", translation_of_interest, "_", file_extension, "_", "EcoSubgroups", "_", time, ".rds")
  
} else if (dataset == "original") {
  
  if (run_subgroups_separately == FALSE) {
    
    filename <- paste0("data/modeling/hdi", "_", dataset, "_", translation_of_interest, "_", file_extension, "_", "EcoSubgroups", "_", time, ".rds")

  } else if (run_subgroups_separately == TRUE) {
    
    filename <- paste0("data/modeling/hdi", "_", dataset, "_", translation_of_interest, "_", group_of_interest, "_", file_extension, "_", "EcoSubgroups", "_", time, ".rds")

  }
  
}

save(hdiHighEcoChoice, hdiLowEcoChoice, file = filename)



