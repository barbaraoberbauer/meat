#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# purpose: plot payne index
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

load("data/behavior/preprocessedDataReplication.RData")

# Calculate within alternative and within attribute transitions ------

dfReplicationProcess <- dfReplicationProcess %>%
  group_by(id, session, consumption_translation, trial, task) %>%
  mutate(
    # extract attribute (remove Eco/NonEco prefix)
    attribute     = str_remove(name, "NonEco") %>% str_remove("Eco"),
    isNonEco      = grepl("NonEco", name, fixed = TRUE),
    
    # compare to previous fixation within trial
    withinAlternative = if_else(fixNum == 1, NA, as.integer(isNonEco == lag(isNonEco))),
    withinAttribute   = if_else(fixNum == 1, NA, as.integer(attribute == lag(attribute)))
  ) %>%
  select(-attribute, -isNonEco) %>%  # remove helper columns if not needed
  ungroup()

# Calculate total number of transitions

dfReplicationProcess <- dfReplicationProcess %>%
  mutate(totalNTransitions = withinAlternative + withinAttribute)

# Add information about number of alternatives and dimensions to data frame

dfReplicationProcess$NumAlternatives <- 2

dfReplicationProcess$NumDimensions[dfReplicationProcess$session == 1] <- 3
dfReplicationProcess$NumDimensions[dfReplicationProcess$session == 2 &
                                     dfReplicationProcess$condition == "control"] <- 3
dfReplicationProcess$NumDimensions[dfReplicationProcess$session == 2 &
                                     dfReplicationProcess$condition != "control"] <- 4 



# Calculate SM Index -----

dfReplicationProcessSubset <- dfReplicationProcess %>%
  filter(fixNum != 1)

# calculate transitions on trial level
dfReplicationProcessSubset <- dfReplicationProcessSubset %>%
  group_by(id, trial, session, consumption_translation) %>%
  summarize(
    withinAlternative = sum(withinAlternative, na.rm = FALSE),
    withinAttribute = sum(withinAttribute, na.rm = FALSE),
    totalNTransitions = sum(totalNTransitions, na.rm = FALSE),
    NumAlternatives = first(NumAlternatives),
    NumDimensions = first(NumDimensions)
  )

# filters trials with no transitions
dfReplicationProcessSubset <- dfReplicationProcessSubset %>%
  filter(totalNTransitions != 0)

# calculate trial-wise index
SMIndex <- dfReplicationProcessSubset %>%
  group_by(id, trial, session, consumption_translation) %>%
  summarize(SM = (sqrt(totalNTransitions) * ((NumAlternatives * NumDimensions/totalNTransitions) * 
                                              (withinAlternative - withinAttribute) - (NumDimensions - NumAlternatives)))/
              (sqrt(NumAlternatives^2 * (NumDimensions - 1) + NumDimensions^2 * (NumAlternatives - 1)))
            )

# calculate participant-wise index
SMIndexAg <- SMIndex %>%
  group_by(id, session, consumption_translation) %>%
  summarize(meanSM = mean(SM))

# Plot Payne Index -----

plotSMIndex <- 
ggplot(data = SMIndexAg,
       aes(x = consumption_translation,
           y = meanSM,
           fill = session)) +
  geom_point(aes(color = session),
             position = position_jitterdodge(dodge.width = 0.5, 
                                             jitter.width = 0.2, 
                                             jitter.height = 0),
             size = 1.1, alpha = 0.7, show.legend = FALSE) +
  stat_summary(fun = mean, 
               geom = "bar",
               linewidth = 1.2,
               position = "dodge",
               width = 0.6, 
               color = "black") +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               position = position_dodge(width = 0.6), 
               width = 0.2, 
               color = "black",
               linewidth = 1.2) + 
  scale_fill_manual(values = scales::alpha(color_sessions, 0.4)) +  
  scale_color_manual(values = color_sessions) +
  scale_x_discrete(labels = labelsReplication) +
  labs(x = "Experimental Condition", 
       y = "SM Index",
       title = "Study 2",
       fill = "Session")
  
# Save plot
ggsave("figures/figureSMIndex.png", 
       plotSMIndex, 
       width = 7,
       height = 5,
       units = "in")


