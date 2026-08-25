#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# purpose: plot group-level parameter estimates, separately for price translation conditions of Study 1
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
              "rjags",
              "dplyr",
              "cowplot")

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
library(rjags)
library(dplyr)
library(cowplot)


# Load functions for plotting
source("R/functions/fun_plot_effects_subgroups.R")

# Load theme
source("R/theme.R")
theme_set(themeMEAT())

rm(package, packages, is_package_installed)


### Load data ------

runJagsOut_absent <- 
  readRDS("data/modeling/runJagsOutmaaDDMDirichlet_original_environmental_friendliness_price_translation_absent_20260824_1737.rds")
runJagsOut_present <- 
  readRDS("data/modeling/runJagsOutmaaDDMDirichlet_original_environmental_friendliness_price_translation_present_20260823_2114.rds")
hdi <- readRDS("data/modeling/hdimaaDDMDirichlet_original_environmental_friendliness_price_translation_subgroups_20260824_1737.rds")

# store as mcmc object
mcmcfin_absent = as.mcmc.list(runJagsOut_absent)
mcmcfin_present = as.mcmc.list(runJagsOut_present)

# combine chains
combined_mcmcfin_absent <- as.data.frame(do.call(rbind, mcmcfin_absent))
combined_mcmcfin_present <- as.data.frame(do.call(rbind, mcmcfin_present))

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


# Calculate Modes -----

modes <- list()

# write function that finds mode
distMode <- function(x){
  as.numeric(names(sort(-table(round(x,3))))[1])
}

# weights
modes$w_price <- list(mode_absent_change = distMode(unname(weights_absent$mu_w_AT_1) -
                                                    combined_mcmcfin_absent$`mu_w[1]`),
                    mode_present_change = distMode(unname(weights_present$mu_w_AT_1) -
                                                     combined_mcmcfin_present$`mu_w[1]`),
                    mode_diff = distMode((unname(weights_present$mu_w_AT_1) -
                                            combined_mcmcfin_present$`mu_w[1]`) - 
                                           (unname(weights_absent$mu_w_AT_1) -
                                              combined_mcmcfin_absent$`mu_w[1]`))
)

modes$w_consumption <- list(mode_absent_change = distMode(unname(weights_absent$mu_w_AT_2) -
                                                          combined_mcmcfin_absent$`mu_w[2]`),
                          mode_present_change = distMode(unname(weights_present$mu_w_AT_2) -
                                                           combined_mcmcfin_present$`mu_w[2]`),
                          mode_diff = distMode((unname(weights_present$mu_w_AT_2) -
                                                  combined_mcmcfin_present$`mu_w[2]`) - 
                                                 (unname(weights_absent$mu_w_AT_2) -
                                                    combined_mcmcfin_absent$`mu_w[2]`))
)

modes$w_popularity <- list(mode_absent_change = distMode(unname(weights_absent$mu_w_AT_3) -
                                                         combined_mcmcfin_absent$`mu_w[3]`),
                         mode_present_change = distMode(unname(weights_present$mu_w_AT_3) -
                                                          combined_mcmcfin_present$`mu_w[3]`),
                         mode_diff = distMode((unname(weights_present$mu_w_AT_3) -
                                                 combined_mcmcfin_present$`mu_w[3]`) - 
                                                (unname(weights_absent$mu_w_AT_3) -
                                                   combined_mcmcfin_absent$`mu_w[3]`))
)


# attentional parameters 


modes$theta <- list(mode_absent_change = distMode(combined_mcmcfin_absent$mu_dtheta),
                  mode_present_change = distMode(combined_mcmcfin_present$mu_dtheta),
                  mode_diff = distMode(combined_mcmcfin_present$mu_dtheta -
                                         combined_mcmcfin_absent$mu_dtheta))

modes$phi <- list(mode_absent_change = distMode(combined_mcmcfin_absent$mu_dphi),
                mode_present_change = distMode(combined_mcmcfin_present$mu_dphi),
                mode_diff = distMode(combined_mcmcfin_present$mu_dphi -
                                       combined_mcmcfin_absent$mu_dphi))

# other parameters

modes$alpha <- list(mode_absent_change = distMode(combined_mcmcfin_absent$mu_dalpha),
                  mode_present_change = distMode(combined_mcmcfin_present$mu_dalpha),
                  mode_diff = distMode(combined_mcmcfin_present$mu_dalpha -
                                         combined_mcmcfin_absent$mu_dalpha))


modes$scaling <- list(mode_absent_change = distMode(combined_mcmcfin_absent$mu_dscaling),
                    mode_present_change = distMode(combined_mcmcfin_present$mu_dscaling),
                    mode_diff = distMode(combined_mcmcfin_present$mu_dscaling -
                                           combined_mcmcfin_absent$mu_dscaling))


modes$tau <- list(mode_absent_change = distMode(combined_mcmcfin_absent$mu_dtau),
                mode_present_change = distMode(combined_mcmcfin_present$mu_dtau),
                mode_diff = distMode(combined_mcmcfin_present$mu_dtau -
                                       combined_mcmcfin_absent$mu_dtau))


modes$sp <- list(mode_absent_change = distMode(combined_mcmcfin_absent$mu_dsp),
                mode_present_change = distMode(combined_mcmcfin_present$mu_dsp),
               mode_diff = distMode(combined_mcmcfin_present$mu_dsp -
                                      combined_mcmcfin_absent$mu_dsp))


# Create plots ------

plots <- list(
  price = plot_effects_subgroups("w_price", "Effects on Weight Price"),
  consumption = plot_effects_subgroups("w_consumption", "Effects on Weight Consumption"),
  popularity = plot_effects_subgroups("w_popularity", "Effects on Weight Popularity"),
  theta = plot_effects_subgroups("theta", "Effects on Theta"),
  phi = plot_effects_subgroups("phi", "Effects on Phi"),
  alpha = plot_effects_subgroups("alpha", "Effects on Boundary Separation"),
  scaling = plot_effects_subgroups("scaling", "Effects on Drift Scaling"),
  tau = plot_effects_subgroups("tau", "Effects on Non-Decision Time"),
  sp = plot_effects_subgroups("sp", "Effects on Starting Point Bias")
)

# combine plots

all_plots <- 
  plots$price +
  plots$consumption +
  plots$popularity +
  plots$theta +
  plots$phi +
  plots$alpha +
  plots$sp +
  plots$tau +
  plots$scaling +
  plot_layout(ncol = 3) +
  plot_annotation(tag_levels = list(c('a', 'b', 'c',  
                                      'd', 'e', 'f', 
                                      'g', 'h', 'i'))) &
  theme(
    plot.margin = margin(8, 8, 8, 8),
    plot.tag = element_text(size = 20, face = "bold")
  )

# save 
ggsave("figures/groupParamEstimates_original_environmental_friendliness_price_translation_subgroups.png",
       all_plots, 
       width = 12, 
       height = 7, 
       units = "in")
