#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# purpose of script: preprocessing
#---

# Preprocessing was largley adapted from Mertens et al. (2020): https://osf.io/jdep3 

# TODO: turn prolific ids into subject number to make data unidentifiable
# TODO: exclude fixations that are slower than 



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

df <- read_csv("data/meat_session1_next50.csv")

### Remove variables that are not needed ------

# variables to drop
drops <- c("id", "expname", "ip", "attributeOrder",  "correctChoice", "value", "jsonfile", "set")
df <- df[ , !(names(df) %in% drops)]
rm(drops)

### Rename variables to match original data ----

df <- df %>% rename(task = fixedTrialNum,
                    condition = condnum,
                    group = condvar,
                    roword = condPermutation,
                    colord = optionOrder)

### Exclude non-conflicting trials ---
df <- filter(df, task != 5 & task != 8 & task != 10)

# Check number of trials
nTrials <- df %>%
  group_by(subject) %>%
  summarize(nTrials = length(unique(task)))

# remove nTrials
#rm(mTrials)

### Re-code to non-eco vs. eco (0 vs. 1) ----

###### Choice ------
  
# A has less emissions and is more ecological option
df$choice[df$cons_A < df$cons_B & df$choice == "A"] <- 1
df$choice[df$cons_A < df$cons_B & df$choice == "B"] <- 0

# B has less emissions and is more ecological option
df$choice[df$cons_B < df$cons_A & df$choice == "B"] <- 1
df$choice[df$cons_B < df$cons_A & df$choice == "A"] <- 0

###### Attribute values -----

df$priceEco <- NaN
df$priceNonEco <- NaN
df$energyEco <- NaN
df$energyNonEco <- NaN
df$popularityEco <- NaN
df$popularityNonEco <- NaN

# price
df$priceEco[df$cons_A < df$cons_B] <- df$price_A[df$cons_A < df$cons_B]
df$priceEco[df$cons_B < df$cons_A] <- df$price_B[df$cons_B < df$cons_A]

df$priceNonEco[df$cons_A < df$cons_B] <- df$price_B[df$cons_A < df$cons_B]
df$priceNonEco[df$cons_B < df$cons_A] <- df$price_A[df$cons_B < df$cons_A]

# consumption
df$energyEco[df$cons_A < df$cons_B] <- df$cons_A[df$cons_A < df$cons_B]
df$energyEco[df$cons_B < df$cons_A] <- df$cons_B[df$cons_B < df$cons_A]

df$energyNonEco[df$cons_A < df$cons_B] <- df$cons_B[df$cons_A < df$cons_B]
df$energyNonEco[df$cons_B < df$cons_A] <- df$cons_A[df$cons_B < df$cons_A]

# popularity
df$popularityEco[df$cons_A < df$cons_B] <- df$pop_A[df$cons_A < df$cons_B]
df$popularityEco[df$cons_B < df$cons_A] <- df$pop_B[df$cons_B < df$cons_A]

df$popularityNonEco[df$cons_A < df$cons_B] <- df$pop_B[df$cons_A < df$cons_B]
df$popularityNonEco[df$cons_B < df$cons_A] <- df$pop_A[df$cons_B < df$cons_A]

###### Event targets -----

# price
df$name[df$cons_A < df$cons_B & df$name == "price_A"] <- "priceEco"
df$name[df$cons_A < df$cons_B & df$name == "price_B"] <- "priceNonEco"

df$name[df$cons_A > df$cons_B & df$name == "price_A"] <- "priceNonEco"
df$name[df$cons_A > df$cons_B & df$name == "price_B"] <- "priceEco"

# consumption
df$name[df$cons_A < df$cons_B & df$name == "cons_A"] <- "energyEco"
df$name[df$cons_A < df$cons_B & df$name == "cons_B"] <- "energyNonEco"

df$name[df$cons_A > df$cons_B & df$name == "cons_A"] <- "energyNonEco"
df$name[df$cons_A > df$cons_B & df$name == "cons_B"] <- "energyEco"

# popularity
df$name[df$cons_A < df$cons_B & df$name == "pop_A"] <- "popularityEco"
df$name[df$cons_A < df$cons_B & df$name == "pop_B"] <- "popularityNonEco"

df$name[df$cons_A > df$cons_B & df$name == "pop_A"] <- "popularityNonEco"
df$name[df$cons_A > df$cons_B & df$name == "pop_B"] <- "popularityEco"

# # price
# df$name[df$cons_A < df$cons_B & df$name == "A1"] <- "priceEco"
# df$name[df$cons_A < df$cons_B & df$name == "B1"] <- "priceNonEco"
# 
# df$name[df$cons_A > df$cons_B & df$name == "A1"] <- "priceNonEco"
# df$name[df$cons_A > df$cons_B & df$name == "B1"] <- "priceEco"
# 
# # consumption
# df$name[df$cons_A < df$cons_B & df$name == "A2"] <- "energyEco"
# df$name[df$cons_A < df$cons_B & df$name == "B2"] <- "energyNonEco"
# 
# df$name[df$cons_A > df$cons_B & df$name == "A2"] <- "energyNonEco"
# df$name[df$cons_A > df$cons_B & df$name == "B2"] <- "energyEco"
# 
# # popularity
# df$name[df$cons_A < df$cons_B & df$name == "A3"] <- "popularityEco"
# df$name[df$cons_A < df$cons_B & df$name == "B3"] <- "popularityNonEco"
# 
# df$name[df$cons_A > df$cons_B & df$name == "A3"] <- "popularityNonEco"
# df$name[df$cons_A > df$cons_B & df$name == "B3"] <- "popularityEco"

df$name[df$name == "A"] <- df$choice[df$name == "A"]
df$name[df$name == "B"] <- df$choice[df$name == "B"]

###### Colord -----

df$colord[df$cons_A < df$cons_B & df$colord == "A-B"] <- "1_0"
df$colord[df$cons_A < df$cons_B & df$colord == "B-A"] <- "0_1"
df$colord[df$cons_A > df$cons_B & df$colord == "A-B"] <- "0_1"
df$colord[df$cons_A > df$cons_B & df$colord == "B-A"] <- "1_0"


# drop A-B coded variables
drops <- c("price_A", "price_B", "cons_A", "cons_B", "pop_A", "pop_B",  
           "emission_A", "emission_B", "rating_A", "rating_B")
df <- df[ , !(names(df) %in% drops)]
rm(drops)

# Exclude participants who used a touchscreen -------

# get subject ids of those who have used touch screen
ids <- unique(df$subject[grepl("touch", df$event)])

exclSubjects <- df %>%
  filter(subject %in% 
           ids) %>%
  distinct(subject) %>%
  mutate(reason = "touchscreen")

# remove subjects
df <- df %>%
  filter(!subject %in% exclSubjects$subject)

# Calculate response times ------

rts <- df %>%
  group_by(subject, task) %>%
  summarise(t_decision = time[event == "btnClick"] - time[event == "onload"])

# add to data frame
df <- df %>%
  left_join(rts, by = c("subject", "task"))

# remove rts
#rm(rts)

# Create aggregated data frame ------

df_aggregate <- df[df$event == "btnClick",]

# remove event columns
drops <- c("event", "name", "time")
df_aggregate <- df_aggregate[ , !(names(df_aggregate) %in% drops)]
rm(drops)

# Inspect process tracing data ------

### Fixation duration -----

events_fixations <- df$event == "mouseover"
events_fixationends <- df$event == "mouseout"

time_onset_fixation = df$time[events_fixations]
time_offset_fixation = df$time[events_fixationends]

# mark fixation events with their fixation number
fixNum <- cumsum(events_fixations)*events_fixations
idxFix <- which(events_fixations)

# calculate fixation duration
fixation_duration <- time_offset_fixation - time_onset_fixation 


### Number of fixations -----

numFixations <- df %>%
  group_by(subject) %>%
  summarize(nFixations = sum(event == "mouseover"))

numFixationsPerAttribute <- df %>%
  group_by(name, subject, task) %>%
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
df_aggregate <- df_aggregate %>%
  left_join(numFixationsPerAttribute, by = c("subject", "task"))

### Calculate total number of fixations ----

# Overall acquisition frequency excluding choice buttons.
tmp <- df_aggregate %>%
  dplyr::select(starts_with('f_')) %>%
  dplyr::mutate(f_total = rowSums(., na.rm = TRUE))
df_aggregate$f_total <- tmp$f_total


  