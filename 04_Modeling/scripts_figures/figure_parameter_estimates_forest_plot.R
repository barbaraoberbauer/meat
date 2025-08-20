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
              "ggplot2",
              "rjags",
              "cowplot",
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
library(ggplot2)
library(rjags)
library(cowplot)
library(runjags)

# Load required functions
source("functions/fun_plot_forest.R")
source("functions/fun_plot_change_forest.R")

rm(package, packages, is_package_installed)


### Load data ------

# specify subset of data 

group_of_interest <- "environmental_friendliness"
# groups: "control", "emissions", "operating_costs", "environmental_friendliness"

# bounded or unbounded attentional parameters? 

bounded <- FALSE # set to TRUE for bounded model, set to FALSE for unbounded model

if (bounded == TRUE) {
  
  file_extension <- "_bounds"
  
} else if (bounded == FALSE) {
  
  file_extension <- "_nobounds"
  
}

filename <- paste0("data/runJagsOut_", group_of_interest, file_extension, ".rds")
runJagsOut <- readRDS(filename)

filename <- paste0("data/hdi_", group_of_interest, file_extension, ".rds")
hdi <- readRDS(filename)

rm(filename)


# Combine Chains in Data -------

# store as mcmc object
mcmcfin = as.mcmc.list(runJagsOut)

# combine chains
combined_mcmcfin <- as.data.frame(do.call(rbind, mcmcfin))


# Calculate Modes -----

# write function that finds mode
distMode <- function(x){
  as.numeric(names(sort(-table(round(x,3))))[1])
}

# boundary
mode_alpha_base <- distMode(combined_mcmcfin$mu_alpha)
mode_alpha_man <- distMode(combined_mcmcfin$mu_alpha + combined_mcmcfin$mu_dalpha)
mode_alpha_change <- distMode(combined_mcmcfin$mu_dalpha)

# scaling
mode_scaling_base <- distMode(combined_mcmcfin$mu_scaling)
mode_scaling_man <- distMode(combined_mcmcfin$mu_scaling + combined_mcmcfin$mu_dscaling)
mode_scaling_change <- distMode(combined_mcmcfin$mu_dscaling)

# tau
mode_tau_base <- distMode(combined_mcmcfin$mu_tau)
mode_tau_man <- distMode(combined_mcmcfin$mu_tau + combined_mcmcfin$mu_dtau)
mode_tau_change <- distMode(combined_mcmcfin$mu_dtau)

# theta
mode_theta_base <- distMode(combined_mcmcfin$mu_theta)
mode_theta_man <- distMode(combined_mcmcfin$mu_theta + combined_mcmcfin$mu_dtheta)
mode_theta_change <- distMode(combined_mcmcfin$mu_dtheta)

# phi
mode_phi_base <- distMode(combined_mcmcfin$mu_phi)
mode_phi_man <- distMode(combined_mcmcfin$mu_phi + combined_mcmcfin$mu_dphi)
mode_phi_change <- distMode(combined_mcmcfin$mu_dphi)

# weight price
mode_w1_base <- distMode(pnorm(combined_mcmcfin$mu_w1))
mode_w1_man <- distMode(pnorm(combined_mcmcfin$mu_w1 + combined_mcmcfin$mu_dw1))
mode_w1_change <- distMode(pnorm(combined_mcmcfin$mu_w1 + combined_mcmcfin$mu_dw1) -
                             pnorm(combined_mcmcfin$mu_w1))

# weight consumption
mode_w2_base <- distMode(pnorm(combined_mcmcfin$mu_w2))
mode_w2_man <- distMode(pnorm(combined_mcmcfin$mu_w2 + combined_mcmcfin$mu_dw2))
mode_w2_change <- distMode(pnorm(combined_mcmcfin$mu_w2 + combined_mcmcfin$mu_dw2) -
                             pnorm(combined_mcmcfin$mu_w2))

# weight popularity
combined_mcmcfin$mu_w3 <- 1 - pnorm(combined_mcmcfin$mu_w1) - pnorm(combined_mcmcfin$mu_w2)
combined_mcmcfin$mu_w3_AT <- 1 - 
  pnorm(combined_mcmcfin$mu_w1 + combined_mcmcfin$mu_dw1) - 
  pnorm(combined_mcmcfin$mu_w2 + combined_mcmcfin$mu_dw2)

mode_w3_base <- distMode(combined_mcmcfin$mu_w3)
mode_w3_man <- distMode(combined_mcmcfin$mu_w3_AT)
mode_w3_change <- distMode(combined_mcmcfin$mu_w3_AT - combined_mcmcfin$mu_w3)

# starting point bias
mode_sp_base <- distMode(combined_mcmcfin$mu_sp)
mode_sp_man <- distMode(combined_mcmcfin$mu_sp + combined_mcmcfin$mu_dsp)
mode_sp_change <- distMode(combined_mcmcfin$mu_dsp)

### Combine to data frame -------
params <- c(mode_alpha_base = mode_alpha_base,
            mode_alpha_man = mode_alpha_man,
            mode_alpha_change = mode_alpha_change,
            mode_scaling_base = mode_scaling_base,
            mode_scaling_man = mode_scaling_man,
            mode_scaling_change = mode_scaling_change,
            mode_tau_base = mode_tau_base,
            mode_tau_man = mode_tau_man,
            mode_tau_change = mode_tau_change,
            mode_theta_base = mode_theta_base,
            mode_theta_man = mode_theta_man,
            mode_theta_change = mode_theta_change,
            mode_phi_base = mode_phi_base,
            mode_phi_man = mode_phi_man,
            mode_phi_change = mode_phi_change,
            mode_w1_base = mode_w1_base,
            mode_w1_man = mode_w1_man,
            mode_w1_change = mode_w1_change,
            mode_w2_base = mode_w2_base,
            mode_w2_man = mode_w2_man,
            mode_w2_change = mode_w2_change,
            mode_w3_base = mode_w3_base,
            mode_w3_man = mode_w3_man,
            mode_w3_change = mode_w3_change,
            mode_sp_base = mode_sp_base,
            mode_sp_man = mode_sp_man,
            mode_sp_change = mode_sp_change)

estimates <- data.frame(parameter = names(params),
                        mode = as.vector(params))

rm(mode_alpha_base,
   mode_alpha_man,
   mode_alpha_change,
   mode_scaling_base,
   mode_scaling_man,
   mode_scaling_change,
   mode_tau_base,
   mode_tau_man,
   mode_tau_change,
   mode_theta_base,
   mode_theta_man,
   mode_theta_change,
   mode_phi_base,
   mode_phi_man,
   mode_phi_change,
   mode_w1_base,
   mode_w1_man,
   mode_w1_change,
   mode_w2_base,
   mode_w2_man,
   mode_w2_change,
   mode_w3_base,
   mode_w3_man,
   mode_w3_change,
   mode_sp_base,
   mode_sp_man,
   mode_sp_change)

estimates <- estimates %>%
  separate(parameter, into = c("prefix", "parameter", "session"), sep = "_", extra = "merge") %>%
  select(-prefix)

estimates$lowerHDI <- NA
estimates$upperHDI <- NA

### Add HDIs -------

estimates[estimates$parameter == "alpha" & estimates$session == "base", 4:5] <- hdi$alpha[[1]] 
estimates[estimates$parameter == "alpha" & estimates$session == "man", 4:5] <- hdi$alpha[[2]] 
estimates[estimates$parameter == "alpha" & estimates$session == "change", 4:5] <- hdi$alpha[[3]] 


estimates[estimates$parameter == "scaling" & estimates$session == "base", 4:5] <- hdi$scaling[[1]] 
estimates[estimates$parameter == "scaling" & estimates$session == "man", 4:5] <- hdi$scaling[[2]] 
estimates[estimates$parameter == "scaling" & estimates$session == "change", 4:5] <- hdi$scaling[[3]] 


estimates[estimates$parameter == "tau" & estimates$session == "base", 4:5] <- hdi$tau[[1]] 
estimates[estimates$parameter == "tau" & estimates$session == "man", 4:5] <- hdi$tau[[2]] 
estimates[estimates$parameter == "tau" & estimates$session == "change", 4:5] <- hdi$tau[[3]] 


estimates[estimates$parameter == "theta" & estimates$session == "base", 4:5] <- hdi$theta[[1]] 
estimates[estimates$parameter == "theta" & estimates$session == "man", 4:5] <- hdi$theta[[2]]
estimates[estimates$parameter == "theta" & estimates$session == "change", 4:5] <- hdi$theta[[3]]

estimates[estimates$parameter == "phi" & estimates$session == "base", 4:5] <- hdi$phi[[1]] 
estimates[estimates$parameter == "phi" & estimates$session == "man", 4:5] <- hdi$phi[[2]] 
estimates[estimates$parameter == "phi" & estimates$session == "change", 4:5] <- hdi$phi[[3]] 

estimates[estimates$parameter == "w1" & estimates$session == "base", 4:5] <- hdi$w_price[[1]] 
estimates[estimates$parameter == "w1" & estimates$session == "man", 4:5] <- hdi$w_price[[2]] 
estimates[estimates$parameter == "w1" & estimates$session == "change", 4:5] <- hdi$w_price[[3]] 

estimates[estimates$parameter == "w2" & estimates$session == "base", 4:5] <- hdi$w_consumption[[1]] 
estimates[estimates$parameter == "w2" & estimates$session == "man", 4:5] <- hdi$w_consumption[[2]] 
estimates[estimates$parameter == "w2" & estimates$session == "change", 4:5] <- hdi$w_consumption[[3]] 

estimates[estimates$parameter == "w3" & estimates$session == "base", 4:5] <- hdi$w_popularity[[1]] 
estimates[estimates$parameter == "w3" & estimates$session == "man", 4:5] <- hdi$w_popularity[[2]] 
estimates[estimates$parameter == "w3" & estimates$session == "change", 4:5] <- hdi$w_popularity[[3]] 

estimates[estimates$parameter == "sp" & estimates$session == "base", 4:5] <- hdi$sp[[1]] 
estimates[estimates$parameter == "sp" & estimates$session == "man", 4:5] <- hdi$sp[[2]] 
estimates[estimates$parameter == "sp" & estimates$session == "change", 4:5] <- hdi$sp[[3]] 

estimates$session <- as.factor(estimates$session)


# Plot Posterior Distributions ------

cols_sess <- c("#225780", "#8CC5E3")

### Boundary ------

pboundary <- plot_params_forest(estimates, "alpha", "Boundary Separation", cols_sess)

### Scaling ------

pscaling <- plot_params_forest(estimates, "scaling", "Drift Scaling", cols_sess)

### Tau ------

ptau <- plot_params_forest(estimates, "tau", "Non-Decision Time", cols_sess)

### Theta ------

ptheta <- plot_params_forest(estimates, "theta", "Discounting Unattended\nOption (theta)", cols_sess)

### Phi ------

pphi <- plot_params_forest(estimates, "phi", "Discounting Unattended\nAttribute (phi)", cols_sess)

### Weight Price ------

pprice <- plot_params_forest(estimates, "w1", "Weight Price", cols_sess)

### Weight Consumption ------

penergy <- plot_params_forest(estimates, "w2", "Weight Consumption", cols_sess)

### Weight Popularity ------

ppopularity <- plot_params_forest(estimates, "w3", "Weight Popularity", cols_sess)

### Starting point -------

psp <- plot_params_forest(estimates, "sp", "Starting Point Bias", cols_sess)


# Plot change parameters -------

cols_change <- "#331832"

# x-axis of change plots
if (group_of_interest == "control") {
  
  change_x_title <- "Difference btw. Sessions"
  
} else {
  
  change_x_title <- "Effect of Attr. Transl."
  
}

# d stands for change (difference)

### Boundary -------

pdBoundary <- plot_change_param_forest(estimates, "alpha", change_x_title, cols_change)

### Scaling ------

pdScaling <- plot_change_param_forest(estimates, "scaling", change_x_title, cols_change)

### Tau ------

pdTau <- plot_change_param_forest(estimates, "tau", change_x_title, cols_change)

### Theta ------

pdTheta <- plot_change_param_forest(estimates, "theta", change_x_title, cols_change)

### Phi ------

pdPhi <- plot_change_param_forest(estimates, "phi", change_x_title, cols_change)


### Weight Price ------

pdPrice <- plot_change_param_forest(estimates, "w1", change_x_title, cols_change)

### Weight Energy ------

pdEnergy <- plot_change_param_forest(estimates, "w2", change_x_title, cols_change)

### Weight Popularity ------

pdPopularity <- plot_change_param_forest(estimates, "w3", change_x_title, cols_change)


### Starting point bias ----

pdsp <- plot_change_param_forest(estimates, "sp", change_x_title, cols_change)

# Arrange plots --------

group_param_estimates <- plot_grid(# plots
  pprice + theme(legend.position = "none"),
  pdPrice,
  penergy + theme(legend.position = "none"),
  pdEnergy,
  ppopularity + theme(legend.position = "none"),
  pdPopularity,
  ptheta + theme(legend.position = "none"),
  pdTheta,
  pphi + theme(legend.position = "none"),
  pdPhi,
  pboundary + theme(legend.position = "none"),
  pdBoundary,
  psp + theme(legend.position = "none"),
  pdsp,
  ptau + theme(legend.position = "none"),
  pdTau,
  pscaling + theme(legend.position = "none"),
  pdScaling,
  
  
  # settings
  ncol = 4,
  labels = c("a", "",
             "b", "",
             "c", "",
             "d", "",
             "e", "",
             "f", "",
             "g", "",
             "h", "",
             "i", ""
  ),
  label_size = 20,
  vjust = 0.9
)

# # get legend
# legend <- get_legend(pboundary +
#                        guides(color = guide_legend(nrow = 1)) +
#                        theme(legend.position = "bottom"))
# 
# # add legend
# group_param_estimates <- plot_grid(group_param_estimates,
#                                    legend,
#                                    ncol = 1,
#                                    rel_heights = c(1, .1))



# save plot
filename <- paste0("figures/group_param_estimates_forest_", group_of_interest, file_extension, ".png")
ggsave(filename, group_param_estimates, width = 12, height = 11)



