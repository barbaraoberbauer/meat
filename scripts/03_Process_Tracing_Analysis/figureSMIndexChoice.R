#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# purpose: plot correlation between sm index and choice
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
              "patchwork",
              "ggpubr")

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
library(ggpubr)

# Load theme
source("R/theme.R")
theme_set(themeMEAT())

rm(package, packages, is_package_installed)


### Load data ---------

load("data/preprocessedDataReplication.RData")

# SM Index --------

### Calculate within alternative and within attribute transitions ------

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



### Calculate SM Index -----

dfReplicationProcessSubset <- dfReplicationProcess %>%
  filter(fixNum != 1)

# calculate transitions on trial level
dfReplicationProcessSubset <- dfReplicationProcessSubset %>%
  group_by(id, trial, task, session, consumption_translation) %>%
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
  group_by(id, trial, task, session, consumption_translation) %>%
  summarize(SM = (sqrt(totalNTransitions) * ((NumAlternatives * NumDimensions/totalNTransitions) * 
                                               (withinAlternative - withinAttribute) - (NumDimensions - NumAlternatives)))/
              (sqrt(NumAlternatives^2 * (NumDimensions - 1) + NumDimensions^2 * (NumAlternatives - 1)))
  )

# calculate participant-wise index
SMIndexAg <- SMIndex %>%
  group_by(id, session, consumption_translation) %>%
  summarize(meanSM = mean(SM))

# calculate condition-wise index
SMIndexConditionAg <- SMIndexAg %>%
  group_by(session, consumption_translation) %>%
  summarize(meanSM = mean(meanSM))


# SM Index and Choice -----

# calculate difference in choice probability across sessions

choiceProbabilities <- dfReplication %>%
  group_by(id, consumption_translation, session) %>%
  summarize(choiceProbability = mean(choice))

choiceProbabilitiesWide <- choiceProbabilities %>%
  pivot_wider(names_from = session,
              values_from = choiceProbability,
              names_prefix = "session_")

choiceProbabilitiesWide$choiceProbDifference <- choiceProbabilitiesWide$session_2 -
  choiceProbabilitiesWide$session_1

# calculate difference in sm index across sessions

SMIndexAgWide <- SMIndexAg %>%
  pivot_wider(names_from = session,
              values_from = meanSM,
              names_prefix = "session_")

SMIndexAgWide$indexDifference <- SMIndexAgWide$session_2 - 
  SMIndexAgWide$session_1

# merge data frames

SMIndexAgWide <- SMIndexAgWide %>%
  left_join(choiceProbabilitiesWide,
            by = c("id", "consumption_translation"))

SMIndexAgWide <- SMIndexAgWide %>%
  filter(!is.na(choiceProbDifference))

SMIndexAgWide %>%
  group_by(consumption_translation) %>%
  summarize(
    cor_test = list(cor.test(choiceProbDifference, indexDifference, 
                             method = "spearman")),
    n = n()
  ) %>%
  mutate(
    rho = sapply(cor_test, function(x) x$estimate),
    S = sapply(cor_test, function(x) x$statistic),
    p_value = sapply(cor_test, function(x) x$p.value),
    df = n - 2
  ) %>%
  select(-cor_test)

# Plot correlation between change in choice and SM index

figureSMIndexChoice <- ggplot(data = SMIndexAgWide,
       aes(x = indexDifference,
           y = choiceProbDifference)) +
  geom_point() +
  labs(x = "SM Index Difference (Session 2 - Session 1)" , 
       y = "Change in Ecological \nChoice Probability \n(Session 2 - Session 1)") +
  coord_cartesian(ylim = c(-1, 1.2)) +
  geom_smooth(method='lm') +
  facet_grid(~consumption_translation,
             labeller = labeller(consumption_translation = labelsReplication)) +
  stat_cor(method = "spearman",
           cor.coef.name = "rho",
           p.digits = 2,
           label.y = 1,
           size = 4) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12,
                                  face = "bold"),  # style for facet labels
        panel.border = element_rect(color = "black", fill = NA)
  )

# Save plot
ggsave("figures/figureSMIndexChoice.png", 
       figureSMIndexChoice, 
       width = 10,
       height = 4,
       units = "in")

save(figureSMIndexChoice,
     file = "figures/figureSMIndexChoice.RData")



