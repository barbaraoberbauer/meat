#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# purpose: plot group-level parameter estimates
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
source("R/functions/fun_plot_forest.R")
source("R/functions/fun_plot_change_forest.R")

# Load theme
source("R/theme.R")
theme_set(themeMEAT())

rm(package, packages, is_package_installed)


### Load data ------

# specify subset of data 

dataset <- "replication"
# datasets: "original", "replication"

translation_of_interest <- "rating_add"
# translations for original dataset: "control", "emissions", "operating_costs", "environmental_friendliness"
# translations for replication dataset: "control", "emission_add", "rating_add", "emission_replace"

run_subgroups_separately <- TRUE
# if set to TRUE, estimates parameters separately for participants that did receive an additional price translation at t2 and those who did not
# only applicable to original data

group_of_interest <- "price_translation_present"
# groups: "price_translation_absent", "price_translation_present"
# only applicable to original data

time <- "20260324_1001"
# time stamp of data generation

# bounded or unbounded attentional parameters? 
bounded <- FALSE # set to TRUE for bounded model, set to FALSE for unbounded model

if (bounded == TRUE) {
  
  file_extension <- "bounds"
  
} else if (bounded == FALSE) {
  
  file_extension <- "nobounds"
  
}

# get runJagsOut and HDI

filename <- paste0("data/modeling/runJagsOut", "_", dataset, "_", translation_of_interest, "_", file_extension, "_", time, ".rds")
runJagsOut <- readRDS(filename)

rm(filename)

filename <- paste0("data/modeling/hdi", "_", dataset, "_", translation_of_interest, "_", file_extension, "_", time, ".rds")
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

### Boundary ------

pboundary <- plot_params_forest(estimates, "alpha", "Boundary Separation", color_sessions)

### Scaling ------

pscaling <- plot_params_forest(estimates, "scaling", "Drift Scaling", color_sessions)

### Tau ------

ptau <- plot_params_forest(estimates, "tau", "Non-Decision Time", color_sessions)

### Theta ------

ptheta <- plot_params_forest(estimates, "theta", "Discounting Unattended\nOption (theta)", color_sessions)

### Phi ------

pphi <- plot_params_forest(estimates, "phi", "Discounting Unattended\nAttribute (phi)", color_sessions)

### Weight Price ------

pprice <- plot_params_forest(estimates, "w1", "Weight Price", color_sessions)

### Weight Consumption ------

penergy <- plot_params_forest(estimates, "w2", "Weight Consumption", color_sessions)

### Weight Popularity ------

ppopularity <- plot_params_forest(estimates, "w3", "Weight Popularity", color_sessions)

### Starting point -------

psp <- plot_params_forest(estimates, "sp", "Starting Point Bias", color_sessions)


# Plot change parameters -------

# x-axis of change plots
if (translation_of_interest == "control") {
  
  change_x_title <- "Difference btw. Sessions"
  
} else {
  
  change_x_title <- "Effect of Attr. Transl."
  
}

# d stands for change (difference)

### Boundary -------

pdBoundary <- plot_change_param_forest(estimates, "alpha", change_x_title, color_change)

### Scaling ------

pdScaling <- plot_change_param_forest(estimates, "scaling", change_x_title, color_change)

### Tau ------

pdTau <- plot_change_param_forest(estimates, "tau", change_x_title, color_change)

### Theta ------

pdTheta <- plot_change_param_forest(estimates, "theta", change_x_title, color_change)

### Phi ------

pdPhi <- plot_change_param_forest(estimates, "phi", change_x_title, color_change)


### Weight Price ------

pdPrice <- plot_change_param_forest(estimates, "w1", change_x_title, color_change)

### Weight Energy ------

pdEnergy <- plot_change_param_forest(estimates, "w2", change_x_title, color_change)

### Weight Popularity ------

pdPopularity <- plot_change_param_forest(estimates, "w3", change_x_title, color_change)


### Starting point bias ----

pdsp <- plot_change_param_forest(estimates, "sp", change_x_title, color_change)

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
  vjust = 1.2
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
filename <- paste0("figures/group_param_estimates_forest", "_", dataset, "_", translation_of_interest, "_", file_extension, "_", time, ".pdf")
ggsave(filename, group_param_estimates, width = 12, height = 11)



