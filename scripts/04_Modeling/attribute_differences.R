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
              "runjags",
              "dplyr")

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


# Load functions written by John Kruschke
# https://github.com/boboppie/kruschke-doing_bayesian_data_analysis/blob/master/2e/DBDA2E-utilities.R
source("functions/DBDA2E-utilities.R")


rm(package, packages, is_package_installed)


### Load data ------

runJagsOut <- readRDS("data/runJagsOut_environmental_friendliness_nobounds.rds")
hdi <- readRDS("data/hdi_environmental_friendliness_nobounds.rds")

### Combine all chains ------

# store as mcmc object
mcmcfin = as.mcmc.list(runJagsOut)

# combine chains
combined_mcmcfin <- as.data.frame(do.call(rbind, mcmcfin))


# Calculate weight differences -------

# calculate mu_w3 (baseline) and mu_w3_AT (manipulation)
combined_mcmcfin$mu_w3 <- 1 - 
  pnorm(combined_mcmcfin$mu_w1) - 
  pnorm(combined_mcmcfin$mu_w2)

combined_mcmcfin$mu_w3_AT <- 1 - 
  pnorm(combined_mcmcfin$mu_w1 + 
          combined_mcmcfin$mu_dw1) - 
  pnorm(combined_mcmcfin$mu_w2 + 
          combined_mcmcfin$mu_dw2)

# price - popularity

pp <- (pnorm(combined_mcmcfin$mu_w1 + combined_mcmcfin$mu_dw1) -
         pnorm(combined_mcmcfin$mu_w1)) -
  (combined_mcmcfin$mu_w3_AT - combined_mcmcfin$mu_w3)


# Calculate modes -------

# write function that finds mode
distMode <- function(x){
  as.numeric(names(sort(-table(round(x,3))))[1])
}

mode_price <- distMode(pnorm(combined_mcmcfin$mu_w1 + combined_mcmcfin$mu_dw1) -
                         pnorm(combined_mcmcfin$mu_w1))

mode_popularity <- distMode(combined_mcmcfin$mu_w3_AT - combined_mcmcfin$mu_w3)

mode_dif <- distMode(pp)


### Combine to data frame -------
params <- c(mode_price = mode_price,
            mode_popularity = mode_popularity,
            mode_dif = mode_dif)

estimates <- data.frame(parameter = names(params),
                        mode = as.vector(params))

estimates <- estimates %>%
  separate(parameter, into = c("prefix", "attribute"), sep = "_", extra = "merge") %>%
  select(-prefix)

estimates$lowerHDI <- NA
estimates$upperHDI <- NA

### Add HDIs -------

estimates[estimates$attribute == "price", 3:4] <- hdi$w_price[[3]]
estimates[estimates$attribute == "popularity", 3:4] <- HDIofMCMC(combined_mcmcfin$mu_w3_AT - combined_mcmcfin$mu_w3)
estimates[estimates$attribute == "dif", 3:4] <- HDIofMCMC(pp)


# Plot attributes -----

cols <- c("#8896AB", "black", "black") 

# linewidth and sizes
lwRegular <- 2.5
lwDif <- 3.5

sizeRegular <- 7
sizeDif <- 10

plot_pp <- ggplot(estimates, aes(x = mode, y = attribute, color = attribute)) +
  geom_vline(xintercept = 0, color = "#CB181D", linetype = "dashed", linewidth = 2) +
  geom_errorbar(aes(xmin=lowerHDI, xmax=upperHDI, linewidth = attribute), width = 0, show.legend = FALSE) +
  scale_linewidth_manual(values = c("price" = lwRegular, "popularity" = lwRegular, "dif" = lwDif)) +
  geom_point(aes(fill = attribute, size = attribute), shape = 22, color = "white", show.legend = FALSE) +
  scale_size_manual(values = c("price" = sizeRegular, "popularity" = sizeRegular, "dif" = sizeDif)) +
  scale_y_discrete(limits = c("dif", "price", "popularity"),
                   labels = c("dif" = "Price -\nPopularity", "price" = "Price", "popularity" = "Popularity")) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  labs(x = "Effect of Attr. Transl.", title = "", color = "Attribute", fill = "Attribute") + 
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.y = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size = 14),
        plot.margin = margin(t = 15,
                             r = 15,
                             b = 15,
                             l = 15)
  ) 


# Save plot -------

ggsave("figures/price-popularity.png", plot_pp)
