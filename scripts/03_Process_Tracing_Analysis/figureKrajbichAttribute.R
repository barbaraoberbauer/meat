#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# purpose: plot relationship between dwell time and choice probability on attribute level
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
              "patchwork")

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
library(patchwork)

# Load theme
source("R/theme.R")
theme_set(themeMEAT())

rm(package, packages, is_package_installed)


### Load data ---------

load("data/preprocessedDataOriginal.RData")
load("data/preprocessedDataReplication.RData")

# Calculate dwell time differences --------

dfOriginal$ddt_consumption <- rowSums(dfOriginal[,c("t_consumption0",
                                                    "t_consumption1",
                                                    "t_consumption_translation0",
                                                    "t_consumption_translation1")],
                                      na.rm = TRUE)

dfOriginal$ddt_consumption_norm <- dfOriginal$ddt_consumption/(dfOriginal$t_option1 + dfOriginal$t_option0)  
dfOriginal$ddt_consumption <- dfOriginal$ddt_consumption/1000 # in sec
  

dfReplication$ddt_consumption <- rowSums(dfReplication[,c("t_consumption0",
                                                    "t_consumption1",
                                                    "t_consumption_translation0",
                                                    "t_consumption_translation1")],
                                      na.rm = TRUE)

dfReplication$ddt_consumption_norm <- dfReplication$ddt_consumption/(dfReplication$t_option1 + dfReplication$t_option0)  
dfReplication$ddt_consumption <- dfReplication$ddt_consumption/1000 # in sec


# Normalized Probability Choosing Eco - Dwell Time on Consumption --------

# number of bins
n_bins <- 10

calculateEcoChoiceDwellTimeConsumption <- function(data){
  
  chooseEcoProbabilityConsumptionNorm <- data %>%
    group_by(session, consumption_translation) %>%
    mutate(bin = ntile(ddt_consumption_norm, n_bins)) %>%
    group_by(session, consumption_translation, bin) %>%
    summarize(binMean = mean(ddt_consumption_norm),
              ecoProb = mean(choice == 1),
              seEcoProb = sqrt(mean(choice == 1) * (1 - mean(choice == 1)) / n()))
  
  return(chooseEcoProbabilityConsumptionNorm)
  
}

chooseEcoProbabilityConsumptionNormOriginal <- calculateEcoChoiceDwellTimeConsumption(dfOriginal)
chooseEcoProbabilityConsumptionNormReplication <- calculateEcoChoiceDwellTimeConsumption(dfReplication)


# Plot dwell time differences and choice probability -------

plotEcoChoiceDwellTime <- function(data, labels, title){
  
  plot <- ggplot(data, aes(x = binMean, y = ecoProb, color = session)) +
    geom_hline(yintercept = 0.5,
               linetype = "dashed",
               color = "darkgrey") +
    geom_point(size = 3) +
    geom_line(linewidth = 0.7) +
    geom_errorbar(aes(ymin = ecoProb - 2 * seEcoProb, 
                      ymax = ecoProb + 2 * seEcoProb),
                  linewidth = 1) +
    scale_color_manual(values = color_sessions) +
    facet_wrap(~consumption_translation, 
               nrow = 2,
               labeller = labeller(consumption_translation = labels)) +
    coord_cartesian(ylim = c(0,1)) + 
    labs(x = "Relative Dwell Time on Consumption (and its Translation)", 
         y = "Probability of Choosing \nMore Ecological Option",
         title = title,
         color = "Session") +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 12,
                                    face = "bold"),  # style for facet labels
          panel.border = element_rect(color = "black", fill = NA)
    )
  
  return(plot)
  
}

plotChooseEcoProbabilityConsumptionNormOriginal <- 
  plotEcoChoiceDwellTime(chooseEcoProbabilityConsumptionNormOriginal, 
                         labelsOriginal,
                         "Original")

plotChooseEcoProbabilityConsumptionNormReplication <- 
  plotEcoChoiceDwellTime(chooseEcoProbabilityConsumptionNormReplication, 
                         labelsReplication,
                         "Replication")

# Combine plots

plotChooseEcoProbabilityConsumptionNorm <- 
  plotChooseEcoProbabilityConsumptionNormOriginal + 
  plot_spacer() +
  plotChooseEcoProbabilityConsumptionNormReplication + 
  plot_layout(widths = c(1, 0.1, 1.5),
              guides = 'collect',
              axis_title = 'collect') &
  theme(legend.position = 'bottom',
        plot.margin = margin(2, 2, 2, 2))


# Save plot
ggsave("figures/figureKrajbichAttribute.png", 
       plotChooseEcoProbabilityConsumptionNorm, 
       width = 10,
       height = 7,
       units = "in")

