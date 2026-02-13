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

df <- df_process[df_process$event == "btnClick",]

# remove event columns
drops <- c("event", "name", "time")
df <- df[ , !(names(df) %in% drops)]
rm(drops)

# Aggregate process tracing data ------

### Fixation duration/Dwell time -----

events_fixations <- df_process$event == "mouseover"
events_fixationends <- df_process$event == "mouseout"

time_onset_fixation = df_process$time[events_fixations]
time_offset_fixation = df_process$time[events_fixationends]

# mark fixation events with their fixation number and add to df
fixNum <- cumsum(events_fixations)*events_fixations
df_process$fixNum <- fixNum

# calculate fixation duration
fixation_duration <- data.frame(fix_duration = time_offset_fixation - time_onset_fixation)
fixation_duration$fixNum <- 1:nrow(fixation_duration) 

# add to df_process
df_process <- df_process %>%
  left_join(fixation_duration, by = c("fixNum"))

# filter fixations below 200 ms
df_process <- df_process %>%
  filter(fix_duration > 200)

# aggregate duration per attribute
durationFixations <- df_process %>%
  group_by(name, id, task) %>%
  summarize(tFixations = round(mean(fix_duration)))

# wide format
durationFixations <- durationFixations %>%
  pivot_wider(names_from = name, values_from = tFixations)

# replace NA with 0 as they indicate no fixations
durationFixations[is.na(durationFixations)] <- 0

# rename columns
durationFixations <- durationFixations %>% 
  rename(t_price1 = priceEco,
         t_price0 = priceNonEco,
         t_consumption1 = energyEco,
         t_consumption0 = energyNonEco,
         t_popularity1 = popularityEco,
         t_popularity0 = popularityNonEco)

# add to data frame
df <- df %>%
  left_join(durationFixations, by = c("id", "task"))

rm(fixation_duration, 
   events_fixationends, 
   events_fixations,
   fixNum,
   time_onset_fixation,
   time_offset_fixation,
   durationFixations)

### Fixation Frequency -----

# calculate number of fixations 
numFixations <- df_process %>%
  group_by(name, id, task) %>%
  summarize(nFixations = sum(event == "mouseover"))


# wide format
numFixations <- numFixations %>%
  pivot_wider(names_from = name, values_from = nFixations)

# replace NA with 0 as they indicate no fixations
numFixations[is.na(numFixations)] <- 0

# rename columns
numFixations <- numFixations %>% 
  rename(f_price1 = priceEco,
         f_price0 = priceNonEco,
         f_consumption1 = energyEco,
         f_consumption0 = energyNonEco,
         f_popularity1 = popularityEco,
         f_popularity0 = popularityNonEco)

# add to data frame
df <- df %>%
  left_join(numFixations, by = c("id", "task"))

rm(numFixations)


# Sample Characteristics ---------
sample <- df[!duplicated(df$id),]
length(sample$id)

remove(sample)

# Computation of Information Acquisition Variables ---------

# Overall acquisition frequency excluding choice buttons.
tmp <- df %>%
  dplyr::select(starts_with('f_')) %>%
  dplyr::mutate(f_total = rowSums(., na.rm = TRUE))
df$f_total <- tmp$f_total

# Overall acquisition duration excluding choice buttons.
tmp <- df %>%
  dplyr::select(starts_with('t_')) %>%
  dplyr::select(-t_decision) %>%
  dplyr::mutate(t_total = rowSums(., na.rm = TRUE))
df$t_total <- tmp$t_total

# Overall acquisition duration per choice option.
tmp <- df %>%
  dplyr::select(t_price0, t_consumption0, t_popularity0) %>%
  dplyr::mutate(t_option0 = rowSums(., na.rm = TRUE))
df$t_option0 <- tmp$t_option0

tmp <- df %>%
  dplyr::select(t_price1, t_consumption1, t_popularity1) %>%
  dplyr::mutate(t_option1 = rowSums(., na.rm = TRUE))
df$t_option1 <- tmp$t_option1

# Relative difference in acquisition duration per choice option (attentional prioritization of choice options).
df <- dplyr::mutate(df, diff_t_options = ((t_option1 - t_option0)/(t_option1 + t_option0)))
remove(tmp)

# Data Cleaning ---------

# Exclude participants who only completed session 1.
# TODO

# Count trials before data cleaning
n_trials_before <- nrow(df)

# Remove trials based on overall decision time and acquisition frequency in session 1.
df_1 <- df %>%
  filter(session == 1) %>%
  mutate(mad = median(t_decision) + 3*(mad(t_decision, center = median(t_decision), constant = 1.4826, na.rm = FALSE, low = FALSE, high = FALSE))) %>%
  filter(t_decision >= 400 & t_decision <= mad) %>%
  filter(f_total >= 2)

# Remove trials based on overall decision time and acquisition frequency in session 2.
df_2 <- df %>%
  filter(session == 2) %>%
  group_by(condition) %>%
  mutate(mad = median(t_decision) + 3*(mad(t_decision, center = median(t_decision), constant = 1.4826, na.rm = FALSE, low = FALSE, high = FALSE))) %>%
  filter(t_decision >= 400 & t_decision <= mad) %>%
  filter(f_total >= 2)

# Merge cleaned dataframes.
df <- bind_rows(df_1, df_2)
remove(df_1, df_2)

n_trials_after <- nrow(df)

rm(n_trials_before, n_trials_after)
