#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# purpose: estimate parameters of maaDDM 2phi model using bayesian hierarchical estimation
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
load("data/behavior/preprocessedDataReplication.RData")

### Specify subset of data ----

dataset <- "original"
# datasets: "original", "replication"

translation_of_interest <- "environmental_friendliness"

# translations for original dataset: "control", "emissions", "operating_costs", "environmental_friendliness"
# translations for replication dataset: "control", "emission_add", "rating_add", "emission_replace", "rating_replace"


# set data

if (dataset == "original") {
  
  df_subset <- dfOriginal
  
} else if (dataset == "replication") {
  
  df_subset <- dfReplication
    
}

# set subset depending on condition
df_subset <- df %>%
  filter(consumption_translation == translation_of_interest)

# assign new ids that are starting from 1 and increment by 1
df_subset <- df_subset %>%
  mutate(id_new = dense_rank(id))

# sort data frame according to id_new (starting from 1 to last participant)
df_subset <- df_subset[order(df_subset$id_new),]


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
  "mu_2phi",
  "sigma_2phi",
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
  "2phi",
  "2phiT",
  "d2phi",
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
  mu_tau = rnorm(1, 0.5, sd)
  sigma_tau = rtruncnorm(1, a = 0, b = Inf, 1, sd)
  mu_scaling = rnorm(1, 1, sd)
  sigma_scaling = rtruncnorm(1, a = 0, b = Inf, 1, sd)
  mu_theta = rnorm(1, 0.5, sd)
  sigma_theta = rtruncnorm(1, a = 0, b = Inf, 1, sd)
  mu_phi = rnorm(1, 0.5, sd)
  sigma_phi = rtruncnorm(1, a = 0, b = Inf, 1, sd)
  mu_2phi = rnorm(1, 0.5, sd)
  sigma_2phi = rtruncnorm(1, a = 0, b = Inf, 1, sd)
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
    mu_2phi = mu_2phi,
    sigma_2phi = sigma_2phi,
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


### Set model specifications ------

nchains <- 6
nAdaptSteps <- 5000
nBurninSteps <- 25000
nUseSteps = nchains * 6500 # total number of used steps
nThinSteps <- 25

### Select model (text file) -----

model_file <- "scripts/04_Modeling/bayes_models/hierarchical_bayesian_maaDDM2phi_dirichlet.txt"


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


### Save model output ------

time <- format(Sys.time(), "%Y%m%d_%H%M")

filename <- paste0("data/modeling/runJagsOutmaaDDM2phiDirichlet", "_", dataset, "_", translation_of_interest, "_", time, ".rds")
  
saveRDS(runJagsOut, file = filename)


# Check results ------

# store as mcmc object
mcmcfin = as.mcmc.list(runJagsOut)

# combine chains
combined_mcmcfin <- as.data.frame(do.call(rbind, mcmcfin))

# set up data frame to store hdis
hdi <- list()

### HDIs weights -------

# Extract posterior samples
mu_w_samples    <- as.matrix(combined_mcmcfin)[, c("mu_w[1]", "mu_w[2]", "mu_w[3]")]
mu_dalr1_samples <- as.matrix(combined_mcmcfin)[, "mu_dalr1"]
mu_dalr2_samples <- as.matrix(combined_mcmcfin)[, "mu_dalr2"]

# Forward transform group mean Session 1 weights to ALR space
mu_alr1 <- log(mu_w_samples[,1] / mu_w_samples[,3])
mu_alr2 <- log(mu_w_samples[,2] / mu_w_samples[,3])

# Add group mean change
mu_alr1_AT <- mu_alr1 + mu_dalr1_samples
mu_alr2_AT <- mu_alr2 + mu_dalr2_samples

# Back-transform to simplex
exp1_AT <- exp(mu_alr1_AT)
exp2_AT <- exp(mu_alr2_AT)
denom_AT <- 1 + exp1_AT + exp2_AT

mu_w_AT_1 <- exp1_AT / denom_AT   # group mean price weight, Session 2
mu_w_AT_2 <- exp2_AT / denom_AT   # group mean energy weight, Session 2
mu_w_AT_3 <- 1       / denom_AT   # group mean popularity weight, Session 2

# Save in HDIs

hdi$w_price <- list(hdi_baseline = HDIofMCMC(combined_mcmcfin$`mu_w[1]`),
                    hdi_manipulation = HDIofMCMC(unname(mu_w_AT_1)),
                    hdi_change = HDIofMCMC(unname(mu_w_AT_1) -
                                                   combined_mcmcfin$`mu_w[1]`))

hdi$w_consumption <- list(hdi_baseline = HDIofMCMC(combined_mcmcfin$`mu_w[2]`),
                          hdi_manipulation = HDIofMCMC(unname(mu_w_AT_2)),
                          hdi_change = HDIofMCMC(unname(mu_w_AT_2) -
                                                         combined_mcmcfin$`mu_w[2]`))

hdi$w_popularity <- list(hdi_baseline = HDIofMCMC(combined_mcmcfin$`mu_w[3]`),
                         hdi_manipulation = HDIofMCMC(unname(mu_w_AT_3)),
                         hdi_change = HDIofMCMC(unname(mu_w_AT_3) -
                                                        combined_mcmcfin$`mu_w[3]`))


### HDIs attentional parameters -------

hdi$theta <- list(hdi_baseline = HDIofMCMC(combined_mcmcfin$mu_theta),
                  hdi_manipulation = HDIofMCMC(combined_mcmcfin$mu_theta + combined_mcmcfin$mu_dtheta),
                  hdi_change = HDIofMCMC(combined_mcmcfin$mu_dtheta))
  
hdi$phi <- list(hdi_baseline = HDIofMCMC(combined_mcmcfin$mu_phi),
                hdi_manipulation = HDIofMCMC(combined_mcmcfin$mu_phi + combined_mcmcfin$mu_dphi),
                hdi_change = HDIofMCMC(combined_mcmcfin$mu_dphi))

hdi$phi2 <- list(hdi_baseline = HDIofMCMC(combined_mcmcfin$mu_phi2),
                hdi_manipulation = HDIofMCMC(combined_mcmcfin$mu_phi2 + combined_mcmcfin$mu_dphi2),
                hdi_change = HDIofMCMC(combined_mcmcfin$mu_dphi2))
  

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

filename <- paste0("data/modeling/hdimaaDDM2phiDirichlet", "_", dataset, "_", translation_of_interest, "_", time, ".rds")

saveRDS(hdi, file = filename)
