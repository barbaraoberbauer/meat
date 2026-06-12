#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# purpose: look at diagnostics of bayesian hierarchical estimation
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
              "runjags",
              "ggplot2")

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
library(ggplot2)

# Load theme
source("R/theme.R")
theme_set(themeMEAT())

# Load function written by John Kruschke for diagnostics
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

run_subgroups_separately <- FALSE
# if set to TRUE, estimates parameters separately for participants that did receive an additional price translation at t2 and those who did not
# only applicable to original data

group_of_interest <- "price_translation_present"
# groups: "price_translation_absent", "price_translation_present"
# only applicable to original data

time <- "20260518_2319"
# time stamp of data generation


# create filename

if (dataset == "replication") {
  
  filename <- paste0("data/modeling/runJagsOutmaaDDMDirichlet", "_", dataset, "_", translation_of_interest, "_", time, ".rds")
  
} else if (dataset == "original") {
  
  if (run_subgroups_separately == FALSE) {
    
    filename <- paste0("data/modeling/runJagsOutmaaDDMDirichlet", "_", dataset, "_", translation_of_interest, "_", time, ".rds")
    
  } else if (run_subgroups_separately == TRUE) {
    
    filename <- paste0("data/modeling/runJagsOutmaaDDMDirichlet", "_", dataset, "_", translation_of_interest, "_", group_of_interest, "_", time, ".rds")
    
  }
  
}

# read data
runJagsOut <- readRDS(filename)

mcmcfin <- as.mcmc.list(runJagsOut)

rm(filename)

# Diagnostics --------

# retrieve summary statistics (including HPD & Rhat/Gelman-Rubin statistic)
# HPD = highest posterior density (HPD) credible interval 
# see: https://cran.r-project.org/web/packages/runjags/runjags.pdf 
summary_statistics <- add.summary(runJagsOut,
                                  vars = c("mu_alpha",    # boundary separation
                                           "mu_dalpha",
                                           "mu_scaling",  # scaling
                                           "mu_dscaling",
                                           "mu_tau",      # non-decision time
                                           "mu_dtau",
                                           "mu_theta",
                                           "mu_dtheta",
                                           "mu_phi",
                                           "mu_dphi",
                                           "mu_w[1]",       # weight price
                                           "mu_w[2]",       # weight sustainability
                                           "mu_w[3]",
                                           "mu_dalr1",
                                           "mu_dalr2",
                                           "kappa",
                                           "mu_sp",       # starting point bias
                                           "mu_dsp"))      

# Rhats (Gelman-Rubin statistic) --------

Rhats <- summary_statistics$psrf

### Plot Rhats -----

# Extract only group-level params (much faster)
group_params <- grep("^mu_", varnames(as.mcmc.list(runJagsOut)), value = TRUE)
mcmc_subset <- as.mcmc.list(runJagsOut)[, group_params]  # subset chains

gr <- gelman.diag(mcmc_subset, multivariate = FALSE)  

param_labels <- c(
  "mu_alpha"    = "Boundary",
  "mu_dalpha"   = "Δ Boundary",
  "mu_theta"    = "Theta",
  "mu_dtheta"   = "Δ Theta",
  "mu_phi"      = "Phi",
  "mu_dphi"     = "Δ Phi",
  "mu_scaling"  = "Scaling",
  "mu_dscaling" = "Δ Scaling",
  "mu_tau"      = "Non-decision Time",
  "mu_dtau"     = "Δ Non-decision Time",
  "mu_w[1]"     = "Weight Price",
  "mu_w[2]"     = "Weight Consumption",
  "mu_w[3]"     = "Weight Popularity",
  "mu_dalr1"    = "Δ Weight Ratio 1",
  "mu_dalr2"    = "Δ Weight Ratio 2",
  "kappa"       = "Kappa",
  "mu_sp"       = "Starting Point",
  "mu_dsp"      = "Δ Starting Point"
)

rhat_df <- data.frame(
  parameter = rownames(gr$psrf),
  point_est = gr$psrf[, 1],
  upper_ci  = gr$psrf[, 2]
)

# Plot
plotRhats <- ggplot(rhat_df, aes(y = reorder(parameter, 
                                point_est))) +
  geom_vline(xintercept = 1.05,
             linewidth = 1.2,
             linetype = "dashed", 
             color = color_error) +
  geom_pointrange(aes(x = point_est, 
                      xmin = point_est,  # lower bound = point estimate 
                      xmax = upper_ci),
                  linewidth = 1,
                  size = 0.8) +
  scale_y_discrete(labels = param_labels) + 
  labs(x = "Rhat", y = NULL, title = "Gelman-Rubin Statistic")


# Monte Carlo Standard Error --------
# acceptable size of MCSE depends, some recommend 5%, others 6.27% 
# of its associated marginal posterior standard deviation

MCSE <- summary_statistics$mcse


# Effective Sample Size --------
# Kruschke (2014) recommends an ESS of 10,000 for accurate and stable 
# estimates of the limits of the 95% HDI

ESS <- MCSE$sseff

# Visual Inspection (following Kruschke, 2014) ------

### Boundary -----

diagMCMC(codaObject = mcmcfin, parName = "mu_dalpha")

openGraph(height=5, width=7)
plotPost(mcmcfin[, "mu_dalpha"], main="mu_dalpha", xlab=bquote(mu_dalpha))


###### Scaling ----

diagMCMC(codaObject = mcmcfin, parName = "mu_dscaling")

openGraph(height=5, width=7)
plotPost(mcmcfin[, "mu_dscaling"], main="mu_dscaling", xlab=bquote(mu_dscaling))


###### Tau -----

diagMCMC(codaObject = mcmcfin, parName = "mu_dtau")

openGraph(height=5, width=7)
plotPost(mcmcfin[, "mu_dtau"], main="mu_dtau", xlab=bquote(mu_dtau))


###### Weight Price ----

diagMCMC(codaObject = mcmcfin, parName = "mu_w[1]")

openGraph(height=5, width=7)
plotPost(mcmcfin[, "mu_w[1]"], main="mu_w[1]", xlab=bquote(mu_w[1]))


###### Weight Energy ----

diagMCMC(codaObject = mcmcfin, parName = "mu_w[2]")

openGraph(height=5, width=7)
plotPost(mcmcfin[, "mu_w[2]"], main="mu_w[2]", xlab=bquote(mu_w[2]))


###### Weight Popularity ----

diagMCMC(codaObject = mcmcfin, parName = "mu_w[3]")

openGraph(height=5, width=7)
plotPost(mcmcfin[, "mu_w[3]"], main="mu_w[3]", xlab=bquote(mu_w[3]))

###### Change Ratio Price / Popularity ----

diagMCMC(codaObject = mcmcfin, parName = "mu_dalr1")

openGraph(height=5, width=7)
plotPost(mcmcfin[, "mu_dalr1"], main="mu_dalr1", xlab=bquote(mu_dalr1))

###### Change Ratio Consumption / Popularity ----

diagMCMC(codaObject = mcmcfin, parName = "mu_dalr2")

openGraph(height=5, width=7)
plotPost(mcmcfin[, "mu_dalr2"], main="mu_dalr2", xlab=bquote(mu_dalr2))





