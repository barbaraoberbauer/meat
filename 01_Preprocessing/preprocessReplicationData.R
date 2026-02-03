#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# purpose of script: preprocessing
#---

# Preprocessing was largley adapted from Mertens et al. (2020): https://osf.io/jdep3 

# TODO: turn prolific ids into subject number to make data unidentifiable



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

df <- read_csv("data/meat_pilot.csv")

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

### Re-code to non-eco vs. eco (0 vs. 1)

# Choice
  
# A has less emissions and is more ecological option
df$choice[df$cons_A < df$cons_B & df$choice == "A"] <- 1
df$choice[df$cons_A < df$cons_B & df$choice == "B"] <- 0

# B has less emissions and is more ecological option
df$choice[df$cons_B < df$cons_A & df$choice == "B"] <- 1
df$choice[df$cons_B < df$cons_A & df$choice == "A"] <- 0
  
# Attribute values
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

# Colord
df$colord[df$cons_A < df$cons_B & df$colord == "A-B"] <- "1_0"
df$colord[df$cons_A < df$cons_B & df$colord == "B-A"] <- "0_1"
df$colord[df$cons_A > df$cons_B & df$colord == "A-B"] <- "0_1"
df$colord[df$cons_A > df$cons_B & df$colord == "B-A"] <- "1_0"

# drop A-B coded variables
drops <- c("price_A", "price_B", "cons_A", "cons_B", "pop_A", "pop_B",  
           "emission_A", "emission_B", "rating_A", "rating_B")
df <- df[ , !(names(df) %in% drops)]
rm(drops)

  