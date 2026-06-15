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
              "runjags",
              "reshape2")

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
library(reshape2)

# Load required functions
source("R/functions/fun_plot_group_means_hdis.R")
source("R/functions/fun_plot_subject_parameter_recovery.R")

rm(package, packages, is_package_installed)


### Load data ------

# specify subset of data 

# datasets: "original", "replication"
dataset <- "replication"

# translations for original dataset: "control", "emissions", "operating_costs", "environmental_friendliness"
# translations for replication dataset: "control", "emission_add", "rating_add", "emission_replace"
translation_of_interest <- "rating_add"

# time stamp of data generation
time <- "20260518_2319"

# load data

filename <- paste0("data/modeling/runJagsOutmaaDDMDirichlet", "_", dataset, "_", translation_of_interest, "_", time, ".rds")
runJagsOut <- readRDS(filename)

filename <- paste0("data/recovery/recoveryResults", "_", dataset, "_", translation_of_interest, ".RData")
load(filename)

# 1 - Ability to correctly infer group mean -----------

### Combine hdi_recoveries and true_parent_parameters -----

infer_group_means <- left_join(hdi_recoveries, 
                  true_parent_parameters,
                  join_by(sim, parameter))

### Create plots ---------

plot_price <- plot_group_means_hdis(infer_group_means, "mu_w[1]", "Weight Price") 

plot_consumption <- plot_group_means_hdis(infer_group_means, "mu_w[2]", "Weight Consumption")

plot_popularity <- plot_group_means_hdis(infer_group_means, "mu_w[3]", "Weight Popularity")

plot_dalr1 <- plot_group_means_hdis(infer_group_means, "mu_dalr1", "Weight Ratio 1 \nChange")
plot_dalr2 <- plot_group_means_hdis(infer_group_means, "mu_dalr2", "Weight Ratio 2 \nChange")

plot_theta <- plot_group_means_hdis(infer_group_means, "mu_theta", "Theta")
plot_dtheta <- plot_group_means_hdis(infer_group_means, "mu_dtheta", "Theta\nChange")

plot_phi <- plot_group_means_hdis(infer_group_means, "mu_phi", "Phi")
plot_dphi <- plot_group_means_hdis(infer_group_means, "mu_dphi", "Phi\nChange")

plot_alpha <- plot_group_means_hdis(infer_group_means, "mu_alpha", "Boundary Separation")
plot_dalpha <- plot_group_means_hdis(infer_group_means, "mu_dalpha", "Boundary Separation\nChange")

plot_scaling <- plot_group_means_hdis(infer_group_means, "mu_scaling", "Drift Scaling")
plot_dscaling <- plot_group_means_hdis(infer_group_means, "mu_dscaling", "Drift Scaling\nChange")

plot_tau <- plot_group_means_hdis(infer_group_means, "mu_tau", "Non-Decision Time")
plot_dtau <- plot_group_means_hdis(infer_group_means, "mu_dtau", "Non-Decision Time\nChange")

plot_sp <- plot_group_means_hdis(infer_group_means, "mu_sp", "Starting Point Bias")
plot_dsp <- plot_group_means_hdis(infer_group_means, "mu_dsp", "Starting Point Bias\nChange")

### Combine plots ------

plot_recovery_hdi <- plot_grid(plot_price, plot_consumption,
                               plot_popularity, ggplot() + theme_void(),
                               plot_dalr1, plot_dalr2,
                               plot_theta, plot_dtheta,
                               plot_phi, plot_dphi,
                               plot_alpha, plot_dalpha,
                               plot_scaling, plot_dscaling,
                               plot_tau, plot_dtau,
                               plot_sp, plot_dsp,
                               ncol = 2)
                                       

# Save plot 
filename <- paste0("figures/figure_recovery_hdi", "_", dataset, "_", translation_of_interest, ".png")
ggsave(filename, plot_recovery_hdi, width = 12, height = 18)

rm(plot_price, plot_consumption,
   plot_popularity,
   plot_dalr1, plot_dalr2,
   plot_theta, plot_dtheta,
   plot_phi, plot_dphi,
   plot_alpha, plot_dalpha,
   plot_scaling, plot_dscaling,
   plot_tau, plot_dtau,
   plot_sp, plot_dsp)


# 2 - Ability to correctly infer individual parameters -------

### Combine subjParameters_recoveries and true_subject_parameters -----

infer_subject_parameter <- left_join(subjParameters_recoveries, 
                               true_subject_parameters,
                               join_by(sim, parameter, subject))

infer_subject_parameter$parameter <- as.factor(infer_subject_parameter$parameter)

### Calculate correlations ------

subject_parameter_correlations <- infer_subject_parameter %>%
  group_by(parameter) %>%
  summarize(#cor_mean = cor(generating_value, means, method = 'pearson'),
            cor_median = cor(generating_value, medians, method = 'pearson'))

### Create plots ---------

plot_subject_price <- plot_subject_parameter_recovery(infer_subject_parameter, 
                                                      "wT1", 
                                                      "Generating Weight Price", 
                                                      "Estimated Weight Price",
                                                      subject_parameter_correlations)

plot_subject_consumption <- plot_subject_parameter_recovery(infer_subject_parameter, 
                                                            "wT2", 
                                                            "Generating Weight Consumption", 
                                                            "Estimated Weight Consumption",
                                                            subject_parameter_correlations)

plot_subject_popularity <- plot_subject_parameter_recovery(infer_subject_parameter, 
                                                       "wT3", 
                                                       "Generating Weight Popularity", 
                                                       "Estimated Weight Popularity",
                                                       subject_parameter_correlations)

plot_subject_theta <- plot_subject_parameter_recovery(infer_subject_parameter, 
                                                      "theta", 
                                                      "Generating Theta", 
                                                      "Estimated Theta",
                                                      subject_parameter_correlations)

plot_subject_dtheta <- plot_subject_parameter_recovery(infer_subject_parameter, 
                                                       "dtheta", 
                                                       "Generating Theta\nChange", 
                                                       "Estimated Theta\nChange",
                                                       subject_parameter_correlations)

plot_subject_phi <- plot_subject_parameter_recovery(infer_subject_parameter, 
                                                    "phi", 
                                                    "Generating Phi", 
                                                    "Estimated Phi",
                                                    subject_parameter_correlations)

plot_subject_dphi <- plot_subject_parameter_recovery(infer_subject_parameter, 
                                                     "dphi", 
                                                     "Generating Phi\nChange", 
                                                     "Estimated Phi\nChange",
                                                     subject_parameter_correlations)

plot_subject_alpha <- plot_subject_parameter_recovery(infer_subject_parameter, 
                                                      "alpha", 
                                                      "Generating Boundary Separation", 
                                                      "Estimated Boundary Separation",
                                                      subject_parameter_correlations)

plot_subject_dalpha <- plot_subject_parameter_recovery(infer_subject_parameter, 
                                                       "dalpha", 
                                                       "Generating Boundary\nSeparation Change", 
                                                       "Estimated Boundary\nSeparation Change",
                                                       subject_parameter_correlations)

plot_subject_scaling <- plot_subject_parameter_recovery(infer_subject_parameter, 
                                                        "scaling", 
                                                        "Generating Drift Scaling", 
                                                        "Estimated Drift Scaling",
                                                        subject_parameter_correlations)

plot_subject_dscaling <- plot_subject_parameter_recovery(infer_subject_parameter, 
                                                         "dscaling", 
                                                         "Generating Drift\nScaling Change", 
                                                         "Estimated Drift\nScaling Change",
                                                         subject_parameter_correlations)

plot_subject_tau <- plot_subject_parameter_recovery(infer_subject_parameter, 
                                                    "tau", 
                                                    "Generating Non-Decision\nTime", 
                                                    "Estimated Non-Decision\nTime",
                                                    subject_parameter_correlations)

plot_subject_dtau <- plot_subject_parameter_recovery(infer_subject_parameter, 
                                                     "dtau", 
                                                     "Generating Non-Decision\nTime Change", 
                                                     "Estimated Non-Decision\nTime Change",
                                                     subject_parameter_correlations)

plot_subject_sp <- plot_subject_parameter_recovery(infer_subject_parameter, 
                                                   "sp", 
                                                   "Generating Starting\nPoint Bias", 
                                                   "Estimated Starting\nPoint Bias",
                                                   subject_parameter_correlations)

plot_subject_dsp <- plot_subject_parameter_recovery(infer_subject_parameter, 
                                                    "dsp", 
                                                    "Generating Starting\nPoint Bias Change", 
                                                    "Estimated Starting\nPoint Bias Change",
                                                    subject_parameter_correlations)

### Combine plots ------

plot_recovery_subject_parameter <- plot_grid(plot_subject_price, plot_subject_consumption,
                               plot_subject_popularity, ggplot() + theme_void(),
                               plot_subject_theta, plot_subject_dtheta,
                               plot_subject_phi, plot_subject_dphi,
                               plot_subject_alpha, plot_subject_dalpha,
                               plot_subject_scaling, plot_subject_dscaling,
                               plot_subject_tau, plot_subject_dtau,
                               plot_subject_sp, plot_subject_dsp,
                               ncol = 2)



# Save plot
filename <- paste0("figures/figure_recovery_subject_parameter", "_", dataset, "_", translation_of_interest, ".png")
ggsave(filename, plot_recovery_subject_parameter, width = 12, height = 17)

rm(plot_subject_price, plot_subject_consumption,
   plot_subject_popularity,
   plot_subject_theta, plot_subject_dtheta,
   plot_subject_phi, plot_subject_dphi,
   plot_subject_alpha, plot_subject_dalpha,
   plot_subject_scaling, plot_subject_dscaling,
   plot_subject_tau, plot_subject_dtau,
   plot_subject_sp, plot_subject_dsp)


# 3 - Correlations between Parent Parameters ---------

mean_cor <- correlation_parents[[1]]
sd_cor <- correlation_parents[[2]]

cor_df <- melt(mean_cor)
sd_df <- melt(sd_cor)
colnames(sd_df) <- c("Var1", "Var2", "sd")

# merge data frames
cor_sd_df <- merge(cor_df, sd_df, by = c("Var1", "Var2"))

# Remove diagonal (where Var1 == Var2)
cor_sd_df <- cor_sd_df %>%
  filter(Var1 != Var2)

# Create combined label
cor_sd_df <- cor_sd_df %>%
  mutate(label = paste0(round(value, 2), " (", round(sd, 2), ")"))

# Filter for the upper triangle if desired (since correlation matrices are symmetric)
cor_sd_df <- cor_sd_df %>%
  filter(as.numeric(factor(Var1, levels=rownames(mean_cor))) <= as.numeric(factor(Var2, levels=colnames(mean_cor))))

plot_parent_correlations <- 
ggplot(cor_sd_df, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#6D9EC1", mid = "white", high = "#E46726", midpoint = 0, limits = c(-1, 1)) +
  geom_text(aes(label = label), size = 2) +
  scale_x_discrete(labels = c("mu_w[2]" = "Weight \n Consumption",
                              "mu_w[3]" = "Weight\n Popularity",
                              "mu_dalr2" = "Weight \n Ratio 1 \nChange",
                              "mu_theta" = "Theta",
                              "mu_dtheta" = "Theta Change",
                              "mu_phi" = "Phi", 
                              "mu_dphi" = "Phi Change",
                              "mu_alpha" = "Boundary\nSeparation",
                              "mu_dalpha" = "Boundary\nSeparation\nChange",
                              "mu_scaling" = "Drift Scaling",
                              "mu_dscaling" = "Drift Scaling\nChange",
                              "mu_tau" = "Non-decision\ntime",
                              "mu_dtau" = "Non-decision\ntime\nChange",
                              "mu_sp" = "Starting point\nbias",
                              "mu_dsp" = "Starting point\nbias Change")) +
  scale_y_discrete(labels = c("mu_w[1]" = "Weight Price",
                              "mu_w[2]" = "Weight \nConsumption",
                              "mu_w[3]" = "Weight\nPopularity",
                              "mu_dalr1" = "Weight \n Ratio 1 \nChange",
                              "mu_dalr2" = "Weight \n Ratio 2 \nChange",
                              "mu_theta" = "Theta",
                              "mu_dtheta" = "Theta Change",
                              "mu_phi" = "Phi", 
                              "mu_dphi" = "Phi Change",
                              "mu_alpha" = "Boundary\nSeparation",
                              "mu_dalpha" = "Boundary\nSeparation\nChange",
                              "mu_scaling" = "Drift Scaling",
                              "mu_dscaling" = "Drift Scaling\nChange",
                              "mu_tau" = "Non-decision\ntime",
                              "mu_dtau" = "Non-decision\ntime\nChange",
                              "mu_sp" = "Starting point\nbias")) +
  theme_bw() +
  labs(x = NULL, y = NULL, title = NULL, fill = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save plot
filename <- paste0("figures/figure_parent_correlations", "_", dataset, "_", translation_of_interest, ".png")
ggsave(filename, plot_parent_correlations, width = 10, height = 6)

