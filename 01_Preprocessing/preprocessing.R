#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# last update: "2025-01-15"
# produced under R version: 2024.09.0
#---

# Preprocessing was largley adapted from Mertens et al. (2020): https://osf.io/jdep3 

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

# Data are available here: https://osf.io/fqdra/ (Study 2)

df <- read_csv("data/mhb_dfx_data_exp2.csv", 
               col_types = (cols('id' = col_character(), 
                                 'submitted' = col_datetime(),
                                 'session' = col_factor(levels = c('1', '2')),
                                 'condition' = col_factor(levels = c('1', '2', '3', '4', '5', '6', '7', '8')),
                                 'consumption_translation' = col_factor(levels = c('control', 'operating_costs', 'emissions', 'environmental_friendliness')),
                                 'price_translation' = col_factor(NULL),
                                 'task' = col_factor(NULL),
                                 'roword' = col_character(),
                                 'colord' = col_character(),
                                 'f_price0' = col_integer(),
                                 'f_price1' = col_integer(),
                                 'f_consumption0' = col_integer(),
                                 'f_consumption1' = col_integer(),
                                 'f_popularity0' = col_integer(),
                                 'f_popularity1' = col_integer(),
                                 'f_consumption_translation0' = col_integer(),
                                 'f_consumption_translation1' = col_integer(),
                                 'f_price_translation0' = col_integer(),
                                 'f_price_translation1' = col_integer(),
                                 'f_choice0' = col_integer(),
                                 'f_choice1' = col_integer(),
                                 't_price0' = col_double(),
                                 't_price1' = col_double(),
                                 't_consumption0' = col_double(),
                                 't_consumption1' = col_double(),
                                 't_popularity0' = col_double(),
                                 't_popularity1' = col_double(),
                                 't_consumption_translation0' = col_double(),
                                 't_consumption_translation1' = col_double(),
                                 't_price_translation0' = col_double(),
                                 't_price_translation1' = col_double(),
                                 't_choice0' = col_double(),
                                 't_choice1' = col_double(),
                                 'maxcount' = col_integer(),
                                 't_decision' = col_integer(),
                                 'choice' = col_integer(),
                                 'gender' = col_factor(NULL),
                                 'age' = col_integer())))

choiceProblems <- readRDS("data/choiceProblems.rds")
# choice problems were extracted from appendix B of publication https://doi.org/10.1017/S1930297500006896 


### Code Book ---------
# id = participant id
# condition:  1 = control condition (price translation absent)
#             2 = control condition (price translation present)
#             3 = operating costs (price translation absent)
#             4 = operating costs (price translation present)
#             5 = carbon emissions (price translation absent)
#             6 = carbon emissions (price translation present)
#             7 = environmental friendliness rating (price translation absent)
#             8 = environmental friendliness rating (price translation present)
#             NA = subject only completed first session
# task = choice problem 1-15
# f ~ acquisition frequency
# t ~ acquisition duration/dwell time
# choice = selection of energy-efficient option (0 = no, 1 = yes)


### Sample Characteristics ---------
sample <- df[!duplicated(df$id),]
length(sample$id)
prop.table(table(sample$gender))
describe(sample$age)

# check sample characteristics for each condition separately 
sample %>%
  group_by(condition) %>%
  do(describe(.$age))

# check sample characteristics for final sample
sample_final <- sample %>% filter(!is.na(condition))
prop.table(table(sample_final$gender))
describe(sample_final$age)

remove(sample, sample_final)

# Computation of Information Acquisition Variables ---------

# Overall acquisition frequency excluding choice buttons.
tmp <- df %>%
  dplyr::select(starts_with('f_')) %>%
  dplyr::select(-f_choice0, -f_choice1) %>%
  dplyr::mutate(f_total = rowSums(., na.rm = TRUE))
df$f_total <- tmp$f_total

# Overall acquisition duration excluding choice buttons.
tmp <- df %>%
  dplyr::select(starts_with('t_')) %>%
  dplyr::select(-t_choice0, -t_choice1, -t_decision) %>%
  dplyr::mutate(t_total = rowSums(., na.rm = TRUE))
df$t_total <- tmp$t_total

# Overall acquisition duration per choice option.
tmp <- df %>%
  dplyr::select(t_price0, t_consumption0, t_popularity0, t_consumption_translation0, t_price_translation0) %>%
  dplyr::mutate(t_option0 = rowSums(., na.rm = TRUE))
df$t_option0 <- tmp$t_option0

tmp <- df %>%
  dplyr::select(t_price1, t_consumption1, t_popularity1, t_consumption_translation1, t_price_translation1) %>%
  dplyr::mutate(t_option1 = rowSums(., na.rm = TRUE))
df$t_option1 <- tmp$t_option1

# Relative difference in acquisition duration per choice option (attentional prioritization of choice options).
df <- dplyr::mutate(df, diff_t_options = ((t_option1 - t_option0)/(t_option1 + t_option0)))
remove(tmp)


# Data Cleaning ---------

# Exclude participants who only completed session 1.
df <- df %>%
  filter(!is.na(condition))

# Count trials before data cleaning
n_trials_before <- nrow(df)

# Remove trials based on overall decision time and acquisition frequency in session 1.
df_1 <- df %>%
  filter(session == 1) %>%
  mutate(mad = median(t_decision) + 3*(mad(t_decision, center = median(t_decision), constant = 1.4826, na.rm = FALSE, low = FALSE, high = FALSE))) %>%
  filter(t_decision >= 400 & t_decision <= mad) %>%
  filter(maxcount >= 2)

# Remove trials based on overall decision time and acquisition frequency in session 2.
df_2 <- df %>%
  filter(session == 2) %>%
  group_by(condition) %>%
  mutate(mad = median(t_decision) + 3*(mad(t_decision, center = median(t_decision), constant = 1.4826, na.rm = FALSE, low = FALSE, high = FALSE))) %>%
  filter(t_decision >= 400 & t_decision <= mad) %>%
  filter(maxcount >= 2)

# Merge cleaned dataframes.
df <- bind_rows(df_1, df_2)
remove(df_1, df_2)

# Remove trials in which participants did not inspect at least two attributes (n = 399)
df <- df %>%
  filter(rowSums(select(., f_price0, 
                          f_price1,
                        f_consumption0,
                        f_consumption1,
                        f_popularity0,
                        f_popularity1,
                        f_consumption_translation0,
                        f_consumption_translation1,
                        f_price_translation0,
                        f_price_translation1), na.rm = TRUE) > 2)

# Remove trials for which total dwell time > total decision time (n = 279)
df <- df %>%
  filter(t_total < t_decision)

n_trials_after <- nrow(df)

rm(n_trials_before, n_trials_after)

# Add choice problem (task) values to df -----

### Re-code task variables from _A/_B to Eco/Non-Eco -----

# assignment of task values to acquisition time of attributes is not ideal
# Task values (Price, Consumption, Popularity) is coded with A and B, 
# the attributes and final choice with 0 and 1 (0 = non-eco, 1 = eco)

taskVariables <- choiceProblems[, c("task",
                                    "Price_A",
                                    "Energy_A",
                                    "Popularity_A",
                                    "Price_B",
                                    "Energy_B",
                                    "Popularity_B")]

taskVariablesEco <- data.frame(task = rep(NA, nrow(taskVariables)),
                               priceEco = rep(NA, nrow(taskVariables)),
                               energyEco = rep(NA, nrow(taskVariables)),
                               popularityEco = rep(NA, nrow(taskVariables)),
                               priceNonEco = rep(NA, nrow(taskVariables)),
                               energyNonEco = rep(NA, nrow(taskVariables)),
                               popularityNonEco = rep(NA, nrow(taskVariables))
) 

# create mask to indicate whether Option B was eco (true) or Option A was eco (false)
mask <- taskVariables$Energy_A > taskVariables$Energy_B


# fill new data frame taskVariablesEco

for (i in 1:nrow(taskVariables)){
  # fill task variable with respective task number (1-15)
  taskVariablesEco$task[i] <- i
  
  # fill task values in correct order, depending on whether Option A or B was the EcoOption
  if (mask[i] == T) {
    
    # if Option B is eco, the elements of B from taskVariables need to be copied to the Eco-Option of taskVariablesEco and vice-versa for A
    taskVariablesEco[i, 2:4] <- taskVariables[i, 5:7]
    taskVariablesEco[i, 5:7] <- taskVariables[i, 2:4]
    
  } else {
    
    # if Option A is eco, the elements from taskVariables (row i, column 2:end) can just be copied
    taskVariablesEco[i, 2:ncol(taskVariables)] <- taskVariables[i, 2:ncol(taskVariables)]
    
  }
  
}

rm(i, mask)

### Normalize task values -----

# Rescale attribute values to equal ranges 
# [1] Berkowitsch et al., 2015: https://bpspsychub.onlinelibrary.wiley.com/doi/10.1111/bmsp.12048 

#function for normalization 
normalizeValue <- function(v_old, min_old, max_old, min_new, max_new){
  
  v_new <- min_new + (v_old - min_old)*(max_new - min_new)/(max_old - min_old)
  return(v_new)
  
}

# note: since the smallest price, energy consumption or popularity score is the best, set 10 to minimal value and 1 to maximum
taskVariablesNormalized <- as.data.frame(apply(taskVariablesEco, 2, function(x) {
  min_x <- min(x)
  max_x <- max(x)
  normalized_column <- sapply(x, function(value) {
    normalizeValue(value, min_x, max_x, 10, 1)  # Normalize to [1, 10]
  })
  return(normalized_column)
}))

# since first column (task-number) was also normalized, set this back to the original task numbers
taskVariablesNormalized$task <- 1:15


### Merge normalized task variables with data frame ------

df <- merge(df, taskVariablesNormalized, by = "task", all.x=T)

saveRDS(taskVariablesNormalized, file = "data/taskVariablesNormalized.rds")


### Select relevant choice problems 
# --> energy & water consumption were of equal amount in choice problems 5, 8, and 10

#data frame with all trials
dfAllTrials <- df

#exclude non-conflicting trials
df <- filter(df, task != 5 & task != 8 & task != 10)


# Save data ----------

saveRDS(df, file = "data/df.rds")
saveRDS(dfAllTrials, file = "data/dfAllTrials.rds")
