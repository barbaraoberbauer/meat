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

df <- read_csv("data/meat_session1_first550.csv")

df <- df[df$expname == "trial", ]

# Clean up data frame -----

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

### Exclude non-conflicting trials ----

df <- filter(df, task != 5 & task != 8 & task != 10)

### Exclude anonymous participants ----
# Anonymous is assigned as subject if no prolific id is provided

df <- df %>%
  filter(
    subject != 'anonymous'
  )

### Unfold process data -----

df <- df %>%
  mutate(parsed = purrr::map(procdata, 
                             ~ read.csv(text = .x, 
                                        stringsAsFactors = FALSE))) %>%
  unnest(parsed)


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

# price
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
           "emission_A", "emission_B", "rating_A", "rating_B",
           "procdata", "addvar", "adddata", "value")
df <- df[ , !(names(df) %in% drops)]
rm(drops)

### Exclude participants who used a touchscreen -------

# get subject ids of those who have used touch screen
ids <- unique(df$subject[grepl("touch", df$event)])

exclSubjects <- df %>%
  filter(subject %in% 
           ids) %>%
  distinct(subject) %>%
  mutate(reason = "touchscreen")

# remove subjects
df <- df %>%
  filter(
    !subject %in% exclSubjects$subject
  )

### Exclude subjects with incomplete data ----

# check number of trials
nTrials <- df %>%
  group_by(subject) %>%
  summarize(nTrials = length(unique(task)))

# get subject ids of those who have less than 12 trials
ids <- unique(nTrials$subject[nTrials$nTrials < 12])

if (length(ids) > 0) {
  exclSubjects <- exclSubjects %>%
    bind_rows(data.frame(subject = ids, 
                         reason = "incomplete data"))
}

# remove subjects
df <- df %>%
  filter(
    !subject %in% exclSubjects$subject
  )

rm(ids)

# Exclude subjects who have refreshed
btnClicksPerTrial <- df %>%
  group_by(subject, task) %>%
  summarize(nBttnClicks = sum(event == "btnClick"))

ids <- unique(btnClicksPerTrial$subject[btnClicksPerTrial$nBttnClicks > 1])

if (length(ids) > 0) {
  exclSubjects <- exclSubjects %>%
    bind_rows(data.frame(subject = ids, 
                         reason = "refresh"))
}

# remove subjects
df <- df %>%
  filter(
    !subject %in% exclSubjects$subject
  )

rm(ids)


# Calculate response times ------

rts <- df %>%
  group_by(subject, task) %>%
  summarise(t_decision = time[event == "btnClick"] - time[event == "onload"])

# add to data frame
df <- df %>%
  left_join(rts, by = c("subject", "task"))

# remove rts
rm(rts)

# calculate number of subjects
nSubjects <- length(unique(df$subject))

# get data for prolific submission
# cutoff <- as.POSIXct("2026-02-10 10:00:00", 
#                      tz = tz(df$submitted))
# submitIds <- unique(df$subject[df$submitted > cutoff])
# write.csv(submitIds, "submission_ids.csv", row.names = FALSE)

# Remove prolific ids -----

df$id <- NA

# assign new ids that are starting from 1 and increment by 1
df <- df %>%
  mutate(id = dense_rank(subject))

# relocate id
df <- df %>%
  relocate(id, .before = session)

# drop prolific id
drops <- c("subject")
df <- df[ , !(names(df) %in% drops)]
rm(drops)

# Save data frames -----
write_csv(df, "data/process_data_replication.csv")

  