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

df_process <- read_csv("data/behavior/process_data_replication.csv",
                       col_types = (cols('id' = col_integer(),
                                         'session' = col_factor(levels = c('1', '2')),
                                         'condition' = col_factor(levels = c('1', '2', '3')),
                                         'group' = col_factor(levels = c('control', 'emission', 'rating')),
                                         'trial' = col_integer(),
                                         'task' = col_factor(NULL),
                                         'roword' = col_character(),
                                         'colord' = col_character(),
                                         'choice' = col_integer(),
                                         'submitted' = col_datetime(),
                                         'event' = col_character(),
                                         'name' = col_character(),
                                         'time' = col_integer(),
                                         'priceEco' = col_double(),
                                         'priceNonEco' = col_double(),
                                         'energyEco' = col_double(),
                                         'energyNonEco' = col_double(),
                                         'popularityEco' = col_double(),
                                         'popularityNonEco' = col_double(),
                                         't_decision' = col_double(),
                                         'age' = col_integer(),
                                         'gender' = col_factor(NULL))))



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

# filter fixations below 200 ms, from A Handbook of Process Tracing Methods, chapter 6
df_process <- df_process %>%
  filter(fix_duration > 200)

# aggregate duration per attribute
durationFixations <- df_process %>%
  group_by(name, id, task, session) %>%
  summarize(tFixations = round(mean(fix_duration)))

# wide format
durationFixations <- durationFixations %>%
  pivot_wider(names_from = name, values_from = tFixations)

# generate new columns for consumption translation
durationFixations <- durationFixations %>%
  mutate(consumptionTranslationEco = coalesce(emissionEco, ratingEco),
         consumptionTranslationNonEco = coalesce(emissionNonEco, ratingNonEco))

# drop unnecessary columns
drops <- c("emissionEco", "emissionNonEco", "ratingEco", "ratingNonEco")
durationFixations <- durationFixations[ , !(names(durationFixations) %in% drops)]
rm(drops)

# rename columns
durationFixations <- durationFixations %>% 
  rename(t_price1 = priceEco,
         t_price0 = priceNonEco,
         t_consumption1 = energyEco,
         t_consumption0 = energyNonEco,
         t_popularity1 = popularityEco,
         t_popularity0 = popularityNonEco,
         t_consumption_translation1 = consumptionTranslationEco,
         t_consumption_translation0 = consumptionTranslationNonEco)

# add to data frame
df <- df %>%
  left_join(durationFixations, by = c("id", "task", "session"))

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
  group_by(name, id, task, session) %>%
  summarize(nFixations = sum(event == "mouseover"))


# wide format
numFixations <- numFixations %>%
  pivot_wider(names_from = name, values_from = nFixations)

# replace NA with 0 as they indicate no fixations
#numFixations[is.na(numFixations)] <- 0

# generate new columns for consumption translation
numFixations <- numFixations %>%
  mutate(consumptionTranslationEco = coalesce(emissionEco, ratingEco),
         consumptionTranslationNonEco = coalesce(emissionNonEco, ratingNonEco))

# drop unnecessary columns
drops <- c("emissionEco", "emissionNonEco", "ratingEco", "ratingNonEco")
numFixations <- numFixations[ , !(names(numFixations) %in% drops)]
rm(drops)

# rename columns
numFixations <- numFixations %>% 
  rename(f_price1 = priceEco,
         f_price0 = priceNonEco,
         f_consumption1 = energyEco,
         f_consumption0 = energyNonEco,
         f_popularity1 = popularityEco,
         f_popularity0 = popularityNonEco,
         f_consumption_translation1 = consumptionTranslationEco,
         f_consumption_translation0 = consumptionTranslationNonEco)

# add to data frame
df <- df %>%
  left_join(numFixations, by = c("id", "task", "session"))

rm(numFixations)

### Replace NA with 0 -----
# Currently, unfixated attributes have NA as duration and fixation count which
# distorts sum and mean calculations (NA are simply excluded)
# Hence, we need to replace NAs with 0s 

# price and popularity were presented in all sessions and conditions and hence NA 
# may be replaced with 0s
df$t_price0[is.na(df$t_price0)] <- 0
df$t_price1[is.na(df$t_price1)] <- 0
df$f_price0[is.na(df$f_price0)] <- 0
df$f_price1[is.na(df$f_price1)] <- 0

df$t_popularity0[is.na(df$t_popularity0)] <- 0
df$t_popularity1[is.na(df$t_popularity1)] <- 0
df$f_popularity0[is.na(df$f_popularity0)] <- 0
df$f_popularity1[is.na(df$f_popularity1)] <- 0

# consumption was present in all first sessions
df$t_consumption0[is.na(df$t_consumption0) & df$session == 1] <- 0
df$t_consumption1[is.na(df$t_consumption1) & df$session == 1] <- 0
df$f_consumption0[is.na(df$f_consumption0) & df$session == 1] <- 0
df$f_consumption1[is.na(df$f_consumption1) & df$session == 1] <- 0

# in all but the replacement groups, consumption was present in session 2
df$t_consumption0[is.na(df$t_consumption0) & df$condition != 2] <- 0
df$t_consumption1[is.na(df$t_consumption1) & df$condition != 2] <- 0
df$f_consumption0[is.na(df$f_consumption0) & df$condition != 2] <- 0
df$f_consumption1[is.na(df$f_consumption1) & df$condition != 2] <- 0

# a consumption translation was present in all second sessions except for the control group
df$t_consumption_translation0[is.na(df$t_consumption_translation0) & df$session == 2 & df$condition != 1] <- 0
df$t_consumption_translation1[is.na(df$t_consumption_translation1) & df$session == 2 & df$condition != 1] <- 0
df$f_consumption_translation0[is.na(df$f_consumption_translation0) & df$session == 2 & df$condition != 1] <- 0
df$f_consumption_translation1[is.na(df$f_consumption_translation1) & df$session == 2 & df$condition != 1] <- 0


# Sample Characteristics ---------
sample <- df[!duplicated(df$id),]
length(sample$id)
prop.table(table(sample$gender))
describe(sample$age)

# check sample characteristics for each condition separately 
sample %>%
  group_by(condition, group) %>%
  do(describe(.$age))

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
  dplyr::select(t_price0, t_consumption0, t_popularity0, t_consumption_translation0) %>%
  dplyr::mutate(t_option0 = rowSums(., na.rm = TRUE))
df$t_option0 <- tmp$t_option0

tmp <- df %>%
  dplyr::select(t_price1, t_consumption1, t_popularity1, t_consumption_translation1) %>%
  dplyr::mutate(t_option1 = rowSums(., na.rm = TRUE))
df$t_option1 <- tmp$t_option1

# Relative difference in acquisition duration per choice option (attentional prioritization of choice options).
df <- dplyr::mutate(df, diff_t_options = ((t_option1 - t_option0)/(t_option1 + t_option0)))
remove(tmp)

# Calculate time between sessions -----

dfSubmitted <- df %>%
  group_by(id, session) %>%
  summarize(dateCompleted = first(submitted))

dfSubmittedWide <- dfSubmitted %>%
  pivot_wider(names_from = session,
              values_from = dateCompleted)

dfSubmittedWide <- dfSubmittedWide %>%
  rename(session1 = `1`, session2 = `2`)

dfSubmittedWide$TimeBetweenSessions <- dfSubmittedWide$session2 - dfSubmittedWide$session1

mean(dfSubmittedWide$TimeBetweenSessions)
sd(dfSubmittedWide$TimeBetweenSessions)

# Data Cleaning ---------

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

# Check number of trials per participant

nTrials <- df %>%
  group_by(id, session) %>%
  summarize(nTrials = length(unique(task)))

# Exclude the above trials from the process tracing data as well

# only keep trials that also exist in df
df_process <- df_process %>%
  semi_join(df, by = c("id", "session", "condition", "group", "trial", "task"))

# assert that number of unique trials is the same for df and df_process
numTrialsDfProcess <- df_process %>%
  group_by(id, session, condition, group) %>%
  summarize(numTrials = n_distinct(trial))

numTrialsDf <- df %>%
  group_by(id, session, condition, group) %>%
  summarize(numTrials = n_distinct(trial))

stopifnot(numTrialsDfProcess$numTrials == numTrialsDf$numTrials)

# Recalculate fixation number after exclusion of trials
df_process <- df_process %>%
  arrange(id, session, condition, group, trial, task, fixNum) %>%
  group_by(id, session, condition, group, trial, task) %>%
  mutate(fixNum = row_number()) %>%
  ungroup()

# Create consumption_translation variable for compatability w original df ----

df$consumption_translation <- NA
df$consumption_translation[df$condition == 1] <- "control"
df$consumption_translation[df$condition == 2 & df$group == "rating"] <- "rating_replace"
df$consumption_translation[df$condition == 3 & df$group == "rating"] <- "rating_add"
df$consumption_translation[df$condition == 2 & df$group == "emission"] <- "emission_replace"
df$consumption_translation[df$condition == 3 & df$group == "emission"] <- "emission_add"

df_process <- df_process %>%
  mutate(
    consumption_translation = 
      df$consumption_translation[match(id, df$id)]
  ) 

df$consumption_translation <- factor(df$consumption_translation, 
                                        levels = c(
                                          "control",
                                          "emission_replace",
                                          "rating_replace",
                                          "emission_add",
                                          "rating_add"
                                          )
                                        )
df_process$consumption_translation <- factor(df_process$consumption_translation,
                                                levels = c(
                                                  "control",
                                                  "emission_replace",
                                                  "rating_replace",
                                                  "emission_add",
                                                  "rating_add"
                                                  )
                                                )


# Save data ----------

dfReplication <- df
dfReplicationProcess <- df_process

save(dfReplication, dfReplicationProcess, file = "data/behavior/preprocessedDataReplication.RData")



