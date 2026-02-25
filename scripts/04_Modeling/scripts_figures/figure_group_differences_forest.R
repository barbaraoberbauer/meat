#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# last update: "2025-05-10"
# produced under R version: 2024.09.0
#---

# plot group differences for consumption weight, theta, boundary separation as forest plots

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
# https://github.com/boboppie/kruschke-doing_bayesian_data_analysis/blob/master/2e/DBDA2E-utilities.R
source("functions/DBDA2E-utilities.R")
source("functions/fun_plot_group_differences_forest.R")

rm(package, packages, is_package_installed)


### Load data ------

runJagsOut_environmental_friendliness <- readRDS("data/runJagsOut_environmental_friendliness_nobounds.rds")
runJagsOut_control <- readRDS("data/runJagsOut_control_nobounds.rds")
runJagsOut_emissions <- readRDS("data/runJagsOut_emissions_nobounds.rds")

hdi_environmental_friendliness <- readRDS("data/hdi_environmental_friendliness_nobounds.rds")
hdi_control <- readRDS("data/hdi_control_nobounds.rds")
hdi_emissions <- readRDS("data/hdi_emissions_nobounds.rds")

### Combine all chains ------

# store as mcmc object
mcmcfin_environmental_friendliness = as.mcmc.list(runJagsOut_environmental_friendliness)
mcmcfin_control = as.mcmc.list(runJagsOut_control)
mcmcfin_emissions = as.mcmc.list(runJagsOut_emissions)


# combine chains
combined_mcmcfin_environmental_friendliness <- as.data.frame(do.call(rbind, mcmcfin_environmental_friendliness))
combined_mcmcfin_control <- as.data.frame(do.call(rbind, mcmcfin_control))
combined_mcmcfin_emissions <- as.data.frame(do.call(rbind, mcmcfin_emissions))


# Calculate group differences -------

### Weight consumption ---------

# rating vs. control group
consumption_rc <- (pnorm(combined_mcmcfin_environmental_friendliness$mu_w2 + combined_mcmcfin_environmental_friendliness$mu_dw2) - 
                     pnorm(combined_mcmcfin_environmental_friendliness$mu_w2)) -
  (pnorm(combined_mcmcfin_control$mu_w2 + combined_mcmcfin_control$mu_dw2) -
     pnorm(combined_mcmcfin_control$mu_w2))
  
# rating vs. carbon emission group
consumption_re <- (pnorm(combined_mcmcfin_environmental_friendliness$mu_w2 + combined_mcmcfin_environmental_friendliness$mu_dw2) -
                     pnorm(combined_mcmcfin_environmental_friendliness$mu_w2)) -
  (pnorm(combined_mcmcfin_emissions$mu_w2 + combined_mcmcfin_emissions$mu_dw2) -
     pnorm(combined_mcmcfin_emissions$mu_w2))

### Theta -------

# rating vs. control group
theta_rc <- combined_mcmcfin_environmental_friendliness$mu_dtheta -
  combined_mcmcfin_control$mu_dtheta

# rating vs. carbon emission group
theta_re <- combined_mcmcfin_environmental_friendliness$mu_dtheta -
  combined_mcmcfin_emissions$mu_dtheta

### Boundary separation -------

# rating vs. control group
alpha_rc <- combined_mcmcfin_environmental_friendliness$mu_dalpha -
  combined_mcmcfin_control$mu_dalpha

# carbon emissions vs. control group
alpha_ec <- combined_mcmcfin_emissions$mu_dalpha -
  combined_mcmcfin_control$mu_dalpha


# Calculate modes -------

# write function that finds mode
distMode <- function(x){
  as.numeric(names(sort(-table(round(x,3))))[1])
}

# boundary
mode_alpha_rating <- distMode(combined_mcmcfin_environmental_friendliness$mu_dalpha)
mode_alpha_control <- distMode(combined_mcmcfin_control$mu_dalpha)
mode_alpha_emissions <- distMode(combined_mcmcfin_emissions$mu_dalpha)
mode_alpha_rc <- distMode(alpha_rc)
mode_alpha_ec <- distMode(alpha_ec)

# theta
mode_theta_rating <- distMode(combined_mcmcfin_environmental_friendliness$mu_dtheta)
mode_theta_control <- distMode(combined_mcmcfin_control$mu_dtheta)
mode_theta_emissions <- distMode(combined_mcmcfin_emissions$mu_dtheta)
mode_theta_rc <- distMode(theta_rc)
mode_theta_re <- distMode(theta_re)

# weight consumption
mode_w2_rating <- distMode(pnorm(combined_mcmcfin_environmental_friendliness$mu_w2 + combined_mcmcfin_environmental_friendliness$mu_dw2) -
                             pnorm(combined_mcmcfin_environmental_friendliness$mu_w2))
mode_w2_control <- distMode(pnorm(combined_mcmcfin_control$mu_w2 + combined_mcmcfin_control$mu_dw2) -
                             pnorm(combined_mcmcfin_control$mu_w2))
mode_w2_emissions <- distMode(pnorm(combined_mcmcfin_emissions$mu_w2 + combined_mcmcfin_emissions$mu_dw2) -
                             pnorm(combined_mcmcfin_emissions$mu_w2))
mode_w2_rc <- distMode(consumption_rc)
mode_w2_re <- distMode(consumption_re)

### Combine to data frame -------
params <- c(mode_alpha_rating = mode_alpha_rating,
            mode_alpha_control = mode_alpha_control,
            mode_alpha_emissions = mode_alpha_emissions,
            mode_alpha_rc = mode_alpha_rc,
            mode_alpha_ec = mode_alpha_ec,
            mode_theta_rating = mode_theta_rating,
            mode_theta_control = mode_theta_control,
            mode_theta_emissions = mode_theta_emissions,
            mode_theta_rc = mode_theta_rc,
            mode_theta_re = mode_theta_re,
            mode_w2_rating = mode_w2_rating,
            mode_w2_control = mode_w2_control,
            mode_w2_emissions = mode_w2_emissions,
            mode_w2_rc = mode_w2_rc,
            mode_w2_re = mode_w2_re)

estimates <- data.frame(parameter = names(params),
                        mode = as.vector(params))

rm(mode_alpha_rating,
   mode_alpha_control,
   mode_alpha_emissions,
   mode_alpha_rc,
   mode_alpha_ec,
   mode_theta_rating,
   mode_theta_control,
   mode_theta_emissions,
   mode_theta_rc,
   mode_theta_re,
   mode_w2_rating,
   mode_w2_control,
   mode_w2_emissions,
   mode_w2_rc,
   mode_w2_re)

estimates <- estimates %>%
  separate(parameter, into = c("prefix", "parameter", "group"), sep = "_", extra = "merge") %>%
  select(-prefix)

estimates$lowerHDI <- NA
estimates$upperHDI <- NA


### Add HDIs -------

estimates[estimates$parameter == "alpha" & estimates$group == "rating", 4:5] <- hdi_environmental_friendliness$alpha[[3]] 
estimates[estimates$parameter == "alpha" & estimates$group == "control", 4:5] <- hdi_control$alpha[[3]] 
estimates[estimates$parameter == "alpha" & estimates$group == "emissions", 4:5] <- hdi_emissions$alpha[[3]] 

estimates[estimates$parameter == "alpha" & estimates$group == "rc", 4:5] <- HDIofMCMC(alpha_rc)
estimates[estimates$parameter == "alpha" & estimates$group == "ec", 4:5] <- HDIofMCMC(alpha_ec)

estimates[estimates$parameter == "theta" & estimates$group == "rating", 4:5] <- hdi_environmental_friendliness$theta[[3]] 
estimates[estimates$parameter == "theta" & estimates$group == "control", 4:5] <- hdi_control$theta[[3]] 
estimates[estimates$parameter == "theta" & estimates$group == "emissions", 4:5] <- hdi_emissions$theta[[3]] 

estimates[estimates$parameter == "theta" & estimates$group == "rc", 4:5] <- HDIofMCMC(theta_rc)
estimates[estimates$parameter == "theta" & estimates$group == "re", 4:5] <- HDIofMCMC(theta_re)

estimates[estimates$parameter == "w2" & estimates$group == "rating", 4:5] <- hdi_environmental_friendliness$w_consumption[[3]] 
estimates[estimates$parameter == "w2" & estimates$group == "control", 4:5] <- hdi_control$w_consumption[[3]] 
estimates[estimates$parameter == "w2" & estimates$group == "emissions", 4:5] <- hdi_emissions$w_consumption[[3]] 

estimates[estimates$parameter == "w2" & estimates$group == "rc", 4:5] <- HDIofMCMC(consumption_rc)
estimates[estimates$parameter == "w2" & estimates$group == "re", 4:5] <- HDIofMCMC(consumption_re)

estimates$group <- as.factor(estimates$group)

# Plot effects of group -----

cols <- c("black", "black", "#8896AB") 
cols_ec <- c("black", "#8896AB", "black")

# linewidth and sizes
lwRegular <- 2.5
lwDif <- 3.5

sizeRegular <- 7
sizeDif <- 10

# labels
groupLabelsRC <- c("rc" = "Rating -\nControl", "rating" = "Rating", "control" = "Control")
valuesLinewidthRC <- c("rating" = lwRegular, "control" = lwRegular, "rc" = lwDif)
valuesSizeRC <- c("rating" = sizeRegular, "control" = sizeRegular, "rc" = sizeDif)

groupLabelsRE <- c("re" = "Rating -\nEmissions", "rating" = "Rating", "emissions" = "Emissions")
valuesLinewidthRE <- c("rating" = lwRegular, "emissions" = lwRegular, "re" = lwDif)
valuesSizeRE <- c("rating" = sizeRegular, "emissions" = sizeRegular, "re" = sizeDif)

groupLabelsEC <- c("ec" = "Emissions -\nControl", "emissions" = "Emissions", "control" = "Control")
valuesLinewidthEC <- c("emissions" = lwRegular, "control" = lwRegular, "ec" = lwDif)
valuesSizeEC <- c("emissions" = sizeRegular, "control" = sizeRegular, "ec" = sizeDif)

### Consumption -------

# rating - control

plot_consumption_rc <- plot_group_differences_forest(estimates, 
                                                     "w2", 
                                                     "rating", 
                                                     "control", 
                                                     "rc",
                                                     valuesLinewidthRC,
                                                     valuesSizeRC,
                                                     groupLabelsRC, 
                                                     "Effects on Weight Consumption", 
                                                     "Rating - Control",
                                                     cols)

# rating - emissions

plot_consumption_re <- plot_group_differences_forest(estimates, 
                                                     "w2", 
                                                     "rating", 
                                                     "emissions", 
                                                     "re",
                                                     valuesLinewidthRE,
                                                     valuesSizeRE,
                                                     groupLabelsRE, 
                                                     "Effects on Weight Consumption",
                                                     "Rating - Emissions",
                                                     cols)



### Theta ---------

# rating - control

plot_theta_rc <- plot_group_differences_forest(estimates, 
                                               "theta", 
                                               "rating", 
                                               "control", 
                                               "rc",
                                               valuesLinewidthRC,
                                               valuesSizeRC,
                                               groupLabelsRC, 
                                               "Effects on Discounting \nUnattended Option (theta)", 
                                               "Rating - Control",
                                               cols)

# rating - emissions

plot_theta_re <- plot_group_differences_forest(estimates, 
                                               "theta", 
                                               "rating", 
                                               "emissions", 
                                               "re",
                                               valuesLinewidthRE,
                                               valuesSizeRE,
                                               groupLabelsRE, 
                                               "Effects on Discounting \nUnattended Option (theta)", 
                                               "Rating - Emissions",
                                               cols)

### Boundary -----

# rating - control

plot_boundary_rc <- plot_group_differences_forest(estimates, 
                                                  "alpha", 
                                                  "rating", 
                                                  "control", 
                                                  "rc",
                                                  valuesLinewidthRC,
                                                  valuesSizeRC,
                                                  groupLabelsRC, 
                                                  "Effects on Boundary Separation", 
                                                  "Rating - Control",
                                                  cols)

# emissions - control

plot_boundary_ec <- plot_group_differences_forest(estimates, 
                                                  "alpha", 
                                                  "emissions", 
                                                  "control", 
                                                  "ec",
                                                  valuesLinewidthEC,
                                                  valuesSizeEC,
                                                  groupLabelsEC, 
                                                  "Effects on Boundary Separation",
                                                  "Emissions - Control",
                                                  cols_ec)




# Arrange plots --------

group_differences <- plot_grid(# plots
  plot_consumption_rc,
  plot_consumption_re,
  plot_theta_rc,
  plot_theta_re,
  plot_boundary_rc,
  plot_boundary_ec,
  
  
  # settings
  ncol = 2,
  labels = c("a", "b",
             "c", "d",
             "e", "f"
  ),
  label_size = 20
)


# Save plot -------

ggsave("figures/group_differences_forest.png", group_differences, width = 10, height = 9)



