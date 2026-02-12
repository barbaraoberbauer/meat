#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# purpose of script: preprocessing
#---

# Load packages and read data ------------

#clear working environment
rm(list=ls())

#clear all plots
if(!is.null(dev.list())) dev.off()


### Install packages -------

# List of packages to check and install if necessary
packages <- c("tidyverse",
              "psych",
              "dplyr"
)

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
library(psych)
library(dplyr)


rm(package, packages, is_package_installed)

### Load data ---------

df_process <- read_csv("data/process_data_replication.csv")

# Create aggregated data frame ------

df <- df[df$event == "btnClick",]

# remove event columns
drops <- c("event", "name", "time")
df <- df[ , !(names(df) %in% drops)]
rm(drops)

# Inspect process tracing data ------

### Fixation duration -----

events_fixations <- df_process$event == "mouseover"
events_fixationends <- df_process$event == "mouseout"

time_onset_fixation = df_process$time[events_fixations]
time_offset_fixation = df_process$time[events_fixationends]

# mark fixation events with their fixation number
fixNum <- cumsum(events_fixations)*events_fixations
idxFix <- which(events_fixations)

# calculate fixation duration
fixation_duration <- time_offset_fixation - time_onset_fixation 


### Number of fixations -----

numFixations <- df_process %>%
  group_by(id) %>%
  summarize(nFixations = sum(event == "mouseover"))

numFixationsPerAttribute <- df_process %>%
  group_by(name, id, task) %>%
  summarize(nFixations = sum(event == "mouseover"))


### Add number of fixations to aggregated -------

# wide format
numFixationsPerAttribute <- numFixationsPerAttribute %>%
  pivot_wider(names_from = name, values_from = nFixations)

# remove columns
drops <- c("0", "1", "body")
numFixationsPerAttribute <- numFixationsPerAttribute[ , !(names(numFixationsPerAttribute) %in% drops)]
rm(drops)

# replace NA with 0 as they indicate no fixations
numFixationsPerAttribute[is.na(numFixationsPerAttribute)] <- 0

# rename columns
numFixationsPerAttribute <- numFixationsPerAttribute %>% 
  rename(f_price1 = priceEco,
         f_price0 = priceNonEco,
         f_consumption1 = energyEco,
         f_consumption0 = energyNonEco,
         f_popularity1 = popularityEco,
         f_popularity0 = popularityNonEco)

# add to data frame
df <- df %>%
  left_join(numFixationsPerAttribute, by = c("id", "task"))

### Calculate total number of fixations ----

# Overall acquisition frequency excluding choice buttons.
tmp <- df %>%
  dplyr::select(starts_with('f_')) %>%
  dplyr::mutate(f_total = rowSums(., na.rm = TRUE))
df$f_total <- tmp$f_total

