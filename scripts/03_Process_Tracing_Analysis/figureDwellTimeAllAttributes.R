#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# purpose: plot dwell times for all attributes
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


# Calculate proportional dwell time -----

calculateFixPropsDif <- function(df, dataset){
  
  # fixProps -> acquistion time for each attribute proportional to the total duration of the trial (vector containing six elements in our case)
  fixProps <- data.frame(price0 = rep(NA, nrow(df)),
                         consumption0 = rep(NA, nrow(df)),
                         popularity0 = rep(NA, nrow(df)),
                         price1 = rep(NA, nrow(df)),
                         consumption1 = rep(NA, nrow(df)),
                         popularity1 = rep(NA, nrow(df))) 
  
  # attributes and their translation are treated as one attribute for simplicity
  # depending on dataset, summarize price and price translation
  if (dataset == "original") {
    
    fixProps$price0 <- 
      rowSums(df[, c("t_price0", "t_price_translation0")], na.rm = TRUE)/1000
    fixProps$price1 <- 
      rowSums(df[, c("t_price1", "t_price_translation1")], na.rm = TRUE)/1000
    
  } else if (dataset == "replication") {
    
    fixProps$price0 <- df$t_price0/1000
    fixProps$price1 <- df$t_price1/1000
    
  }
  
  fixProps$consumption0 <- df$t_consumption0/1000 # do not add dwell time on translation as we are interested in the original attribute
  fixProps$popularity0 <- df$t_popularity0/1000
  fixProps$consumption1 <- df$t_consumption1/1000
  fixProps$popularity1 <- df$t_popularity1/1000
  
  # divide by total duration of the trial
  #fixProps <- fixProps/abs(df$t_decision) #take absolute value instead of +/- coded RT
  fixProps <- fixProps/df$t_total # divide by total dwell time
  
  # normalize each trial to 1
  fixProps <- fixProps/rowSums(fixProps) 
  
  # add info about id and session
  fixProps$id <- df$id
  fixProps$session <- df$session
  fixProps$consumption_translation <- df$consumption_translation
  
  # aggregate data over subjects
  fixPropsAgg <- fixProps %>%
    group_by(id, session, consumption_translation) %>%
    summarize(price0 = mean(price0, na.rm = TRUE),
              consumption0 = mean(consumption0, na.rm = TRUE),
              popularity0 = mean(popularity0, na.rm = TRUE),
              price1 = mean(price1, na.rm = TRUE),
              consumption1 = mean(consumption1, na.rm = TRUE),
              popularity1 = mean(popularity1, na.rm = TRUE))
  
  # turn into appropriate data frame
  fixPropsAggWide <- fixPropsAgg %>%
    pivot_wider(
      id_cols = c(id, consumption_translation),
      names_from = session,
      values_from = c(price0, 
                      consumption0, 
                      popularity0, 
                      price1, 
                      consumption1, 
                      popularity1),
      names_glue = "{.value}_{session}"
    )
  
  # calculate session difference
  fixPropsDif <- fixPropsAggWide %>%
    group_by(id, consumption_translation) %>%
    summarize(price0_diff = price0_2 - price0_1,
              consumption0_diff = consumption0_2 - consumption0_1,
              popularity0_diff = popularity0_2 - popularity0_1,
              price1_diff = price1_2 - price1_1,
              consumption1_diff = consumption1_2 - consumption1_1,
              popularity1_diff = popularity1_2 - popularity1_1)
  
  # bring into long format
  fixPropsDif_long <- fixPropsDif %>%
    pivot_longer(
      cols = "price0_diff":"popularity1_diff",
      names_to = "attribute",
      values_to = "propDwellTimeDiff"
    )
  
  fixPropsDif_long$attribute <- 
    factor(fixPropsDif_long$attribute, levels = c("price0_diff",
                                                  "price1_diff",
                                                  "consumption0_diff",
                                                  "consumption1_diff",
                                                  "popularity0_diff",
                                                  "popularity1_diff"))
  
  
  return(fixPropsDif_long)
  
  
  
}

fixPropsDifOriginal <- calculateFixPropsDif(dfOriginal, "original")
fixPropsDifReplication <- calculateFixPropsDif(dfReplication, "replication")


# Plot proportional dwell time changes --------

attribute_labels <- c(
  "price0_diff" = "Price \nOther",
  "price1_diff" = "Price \nEco",
  "consumption0_diff" = "Consum \nOther",
  "consumption1_diff" = "Consum \nEco",
  "popularity0_diff" = "Popularity \nOther",
  "popularity1_diff" = "Popularity \nEco")


plotDwellTimes <- function(df, title, labels){
  
  plotDwellTime <- 
    ggplot(df, 
           aes(x = attribute, 
               y = propDwellTimeDiff)) +
    geom_hline(yintercept = 0, 
               color = "darkred", 
               linewidth = 2, 
               linetype = "dashed") +
    stat_summary(fun = "mean", 
                 size = 0.5,
                 position = position_dodge(width = 0.3)) +
    stat_summary(fun.data = mean_se, 
                 geom = "errorbar", 
                 position = position_dodge(width = 0.3),
                 linewidth = 1.2,
                 width = 0.15) +
    facet_grid(~ consumption_translation, 
               labeller = labeller(consumption_translation = labels)) +
    scale_x_discrete(labels = attribute_labels) +
    labs(x = "Attributes", 
         y = "Dwell Time Differences \n(Session 2 - Session 1)",
         title = title) +
    theme(axis.text.x = element_text(angle = 90, 
                                     vjust = 0.5, 
                                     hjust=1),
          strip.background = element_blank(),
          strip.text = element_text(size = 12,
                                    face = "bold"),  # style for facet labels
          panel.border = element_rect(color = "black", fill = NA))
  
  return(plotDwellTime)
  
}

plotOriginal <- plotDwellTimes(fixPropsDifOriginal, "Study 1", labelsOriginal)

plotReplication <- plotDwellTimes(fixPropsDifReplication, "Study 2", labelsReplication)





