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
              "patchwork",
              "grid",
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
library(patchwork)
library(grid)
library(runjags)

# Load required functions
source("R/functions/fun_plot_parameter_densities.R")
source("R/functions/fun_plot_parameter_change.R")


# Load theme
source("R/theme.R")
theme_set(themeMEAT())

rm(package, packages, is_package_installed)


### Load data ------

# specify subset of data 

# translations of interest: control, emissions, environmental_friendliness (these conditions are identical across studies)
translation_of_interest <- "environmental_friendliness"

# time stamp of data generation
time_original <- "20260519_0532"
time_replication <- "20260518_2319"


# get runJagsOut and HDI

# set names of conditions for each study
if (translation_of_interest == "environmental_friendliness") {
  
  toi_original <- "environmental_friendliness"
  toi_replication <- "rating_add"
  
  xtitleChangePlot <- "Effect of Attr. Transl."
  
} else if (translation_of_interest == "emissions") {
  
  toi_original <- "emissions"
  toi_replication <- "emission_add"
  
  xtitleChangePlot <- "Effect of Attr. Transl."
  
} else if (translation_of_interest == "control") {
  
  toi_original <- "control"
  toi_replication <- "control"
  
  xtitleChangePlot <- "Difference btw. Sessions"
  
}

filenameOriginal <- paste0("data/modeling/runJagsOutmaaDDMDirichlet_original", "_", 
                           toi_original, "_", 
                           time_original, ".rds")

filenameReplication <- paste0("data/modeling/runJagsOutmaaDDMDirichlet_replication", "_", 
                           toi_replication, "_", 
                           time_replication, ".rds")

filenameHDIOriginal <- paste0("data/modeling/hdimaaDDMDirichlet_original", "_", 
                              toi_original, "_", 
                              time_original, ".rds")

filenameHDIReplication <- paste0("data/modeling/hdimaaDDMDirichlet_replication", "_", 
                              toi_replication, "_", 
                              time_replication, ".rds")

# load files
runJagsOutOriginal <- readRDS(filenameOriginal)
runJagsOutReplication <- readRDS(filenameReplication)

hdiOriginal <- readRDS(filenameHDIOriginal)
hdiReplication <- readRDS(filenameHDIReplication)

rm(filenameOriginal, filenameReplication, filenameHDIOriginal, filenameHDIReplication)

# Generate density and change plots ------

generate_all_plots <- function(runJagsOut, hdi) {
  
  # Combine Chains in Data 
  
  # store as mcmc object
  mcmcfin = as.mcmc.list(runJagsOut)
  
  # combine chains
  combined_mcmcfin <- as.data.frame(do.call(rbind, mcmcfin))
  
  # Extract weights 
  
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
  
  
  # Group mean density plots
  plots_density <- list(
    price = plot_group_means_densities(combined_mcmcfin,
                                        combined_mcmcfin$`mu_w[1]`,
                                        mu_w_AT_1,
                                        "Weight Price"),
    consumption = plot_group_means_densities(combined_mcmcfin,
                                              combined_mcmcfin$`mu_w[2]`,
                                              mu_w_AT_2,
                                              "Weight Consumption"),
    popularity = plot_group_means_densities(combined_mcmcfin,
                                             combined_mcmcfin$`mu_w[3]`,
                                             mu_w_AT_3,
                                             "Weight Popularity"),
    boundary = plot_group_means_densities(combined_mcmcfin, 
                                           combined_mcmcfin$mu_alpha, 
                                           combined_mcmcfin$mu_alpha + combined_mcmcfin$mu_dalpha, 
                                           "Boundary Separation"),
    theta = plot_group_means_densities(combined_mcmcfin,
                                        combined_mcmcfin$mu_theta,
                                        combined_mcmcfin$mu_theta + combined_mcmcfin$mu_dtheta,
                                        "Discounting Unattended \nOption (theta)"),
    phi = plot_group_means_densities(combined_mcmcfin,
                                      combined_mcmcfin$mu_phi,
                                      combined_mcmcfin$mu_phi + combined_mcmcfin$mu_dphi,
                                      "Discounting Unattended \nAttribute (phi)"),
    sp = plot_group_means_densities(combined_mcmcfin,
                                     combined_mcmcfin$mu_sp,
                                     combined_mcmcfin$mu_sp + combined_mcmcfin$mu_dsp,
                                     "Starting Point Bias"),
    ndt = plot_group_means_densities(combined_mcmcfin,
                                      combined_mcmcfin$mu_tau,
                                      combined_mcmcfin$mu_tau + combined_mcmcfin$mu_dtau,
                                      "Non-Decision Time"),
    scaling = plot_group_means_densities(combined_mcmcfin,
                                          combined_mcmcfin$mu_scaling,
                                          combined_mcmcfin$mu_scaling + combined_mcmcfin$mu_dscaling,
                                          "Drift Scaling")
  ) # density plots
  
  
  # Change parameter plots
  plots_change <- list(
    dPrice = plot_change_param(mu_w_AT_1 - combined_mcmcfin$`mu_w[1]`,
                                hdi$w_price$hdi_change,
                                xtitleChangePlot),
    dConsumption = plot_change_param(mu_w_AT_2 - combined_mcmcfin$`mu_w[2]`,
                                      hdi$w_consumption$hdi_change,
                                      xtitleChangePlot),
    dPopularity = plot_change_param(mu_w_AT_3 - combined_mcmcfin$`mu_w[3]`,
                                     hdi$w_popularity$hdi_change,
                                     xtitleChangePlot),
    dBoundary = plot_change_param(combined_mcmcfin$mu_dalpha,
                                   hdi$alpha$hdi_change,
                                   xtitleChangePlot),
    dTheta = plot_change_param(combined_mcmcfin$mu_dtheta,
                                hdi$theta$hdi_change,
                                xtitleChangePlot),
    dPhi = plot_change_param(combined_mcmcfin$mu_dphi,
                              hdi$phi$hdi_change,
                              xtitleChangePlot),
    dSp = plot_change_param(combined_mcmcfin$mu_dsp,
                             hdi$sp$hdi_change,
                             xtitleChangePlot),
    dNdt = plot_change_param(combined_mcmcfin$mu_dtau,
                              hdi$tau$hdi_change,
                              xtitleChangePlot),
    dScaling = plot_change_param(combined_mcmcfin$mu_dscaling,
                                  hdi$scaling$hdi_change,
                                  xtitleChangePlot)
  ) # change plots
  
  list(density = plots_density,
       change = plots_change)
  
}

# Run for both datasets
plots_original <- generate_all_plots(runJagsOutOriginal, hdiOriginal)
plots_replication <- generate_all_plots(runJagsOutReplication, hdiReplication)


# Combine plots -------

### Weights and Attention -----

original_label <- wrap_elements(
  grid::textGrob("Study 1", 
                 #hjust = 0.5,
                 #vjust = 1,  
                 gp = grid::gpar(fontsize = 20, fontface = "bold"))
)

replication_label <- wrap_elements(
  grid::textGrob("Study 2", 
                 #hjust = 0.5,
                 #vjust = 1,  
                 gp = grid::gpar(fontsize = 20, fontface = "bold"))
)

header <- original_label + replication_label 

# plot weights
weights_attention <-
  # price
  plots_original$density$price +
  plots_original$change$dPrice +
  plots_replication$density$price +
  plots_replication$change$dPrice +
  # consumption
  plots_original$density$consumption +
  plots_original$change$dConsumption +
  plots_replication$density$consumption +
  plots_replication$change$dConsumption +
  # popularity
  plots_original$density$popularity +
  plots_original$change$dPopularity +
  plots_replication$density$popularity +
  plots_replication$change$dPopularity +
  # theta
  plots_original$density$theta +
  plots_original$change$dTheta +
  plots_replication$density$theta +
  plots_replication$change$dTheta +
  # phi
  plots_original$density$phi +
  plots_original$change$dPhi +
  plots_replication$density$phi +
  plots_replication$change$dPhi +
  plot_layout(ncol = 4,
              guides = 'collect')
  

weight_attention_parameters <- 
  (header / weights_attention) +
  plot_layout(heights = c(1, 15), guides = 'collect') +
  plot_annotation(
    tag_levels = list(c('', '',
                        'a', '', 'b', '',
                        '', '', '', '',
                        '', '', '', '',
                        'c', '', 'd', '',
                        'e', '', 'f', ''))
  ) &
  theme(
    plot.margin = margin(4, 4, 4, 4),
    legend.position = "bottom",
    plot.tag = element_text(size = 20, face = "bold")
  )


### Speed-accuracy parameters ----

speed_accuracy <-
  # boundary separation
  plots_original$density$boundary +
  plots_original$change$dBoundary +
  plots_replication$density$boundary +
  plots_replication$change$dBoundary +
  # starting point bias
  plots_original$density$sp +
  plots_original$change$dSp +
  plots_replication$density$sp +
  plots_replication$change$dSp +
  # non-decision time
  plots_original$density$ndt +
  plots_original$change$dNdt +
  plots_replication$density$ndt +
  plots_replication$change$dNdt +
  # drift scaling
  plots_original$density$scaling +
  plots_original$change$dScaling +
  plots_replication$density$scaling +
  plots_replication$change$dScaling +
  plot_layout(ncol = 4,
              guides = 'collect')


speed_accuracy_parameters <- 
  (header / speed_accuracy) +
  plot_layout(heights = c(1, 15), guides = 'collect') +
  plot_annotation(
    tag_levels = list(c('', '',
                        'a', '', 'b', '',
                        'c', '', 'd', '',
                        'e', '', 'f', '',
                        'g', '', 'h', ''))
  ) &
  theme(
    plot.margin = margin(4, 4, 4, 4),
    legend.position = "bottom",
    plot.tag = element_text(size = 20, face = "bold")
  )


# Combine all -----

all_plots <- 
  # price
  plots_original$density$price + 
  plots_original$change$dPrice +
  plots_replication$density$price +
  plots_replication$change$dPrice +
  # consumption
  plots_original$density$consumption + 
  plots_original$change$dConsumption +
  plots_replication$density$consumption +
  plots_replication$change$dConsumption +
  # popularity
  plots_original$density$popularity + 
  plots_original$change$dPopularity +
  plots_replication$density$popularity +
  plots_replication$change$dPopularity +
  # theta
  plots_original$density$theta +
  plots_original$change$dTheta +
  plots_replication$density$theta +
  plots_replication$change$dTheta +
  # phi
  plots_original$density$phi +
  plots_original$change$dPhi +
  plots_replication$density$phi +
  plots_replication$change$dPhi +
  # boundary separation
  plots_original$density$boundary +
  plots_original$change$dBoundary +
  plots_replication$density$boundary +
  plots_replication$change$dBoundary +
  # starting point bias
  plots_original$density$sp +
  plots_original$change$dSp +
  plots_replication$density$sp +
  plots_replication$change$dSp +
  # non-decision time
  plots_original$density$ndt +
  plots_original$change$dNdt +
  plots_replication$density$ndt +
  plots_replication$change$dNdt +
  # drift scaling
  plots_original$density$scaling +
  plots_original$change$dScaling +
  plots_replication$density$scaling +
  plots_replication$change$dScaling +
  plot_layout(ncol = 4,
              guides = 'collect') 


all_parameters <- 
  (header / all_plots) +
  plot_layout(heights = c(1, 15), guides = 'collect') +
  plot_annotation(
    tag_levels = list(c('', '',
                        'a', '', 'b', '',
                        'c', '', 'd', '',
                        'e', '', 'f', '',
                        'g', '', 'h', '',
                        'i', '', 'j', '',
                        'k', '', 'l', '',
                        'm', '', 'n', '',
                        'o', '', 'p', '',
                        'q', '', 'r', ''))
  ) &
  theme(
    plot.margin = margin(4, 4, 4, 4),
    legend.position = "bottom",
    plot.tag = element_text(size = 20, face = "bold")
  )



# Save plots --------

# save weights and attention plot
filename <- paste0("figures/groupParamEstimatesBothStudiesWeightsAttention", "_", translation_of_interest, ".png")
ggsave(filename, 
       weight_attention_parameters, 
       width = 11, height = 11)


# save speed accuracy plot
filename <- paste0("figures/groupParamEstimatesBothStudiesSpeedAccuracy", "_", translation_of_interest, ".png")
ggsave(filename, 
       speed_accuracy_parameters, 
       width = 11, height = 8.8)


# save all parameters
filename <- paste0("figures/groupParamEstimatesBothStudiesAllParameters", "_", translation_of_interest, ".png")
ggsave(filename, all_parameters, width = 11, height = 19.8, units = "in")



