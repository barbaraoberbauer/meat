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

# specify modeling data

dataset <- "replication"
# datasets: only possible for replication data set

translation_of_interest <- "rating_add"
# translations for replication dataset: "control", "emission_add", "rating_add", "emission_replace", "rating_replace"

time <- "20260518_2319"
# time stamp of data generation

filename <- paste0("data/modeling/runJagsOutmaaDDMDirichlet", "_", dataset, "_", translation_of_interest, "_", time, ".rds")
runJagsOut <- readRDS(filename)


# Set sample -------

df_subset <- dfReplicationProcess %>%
  filter(consumption_translation == translation_of_interest)

# assign new ids that are starting from 1 and increment by 1
df_subset <- df_subset %>%
  mutate(id_new = dense_rank(id))

# sample size
SampleSize <- length(unique(df_subset$id_new))


# Derive attribute weights -----

# store as mcmc object
mcmcfin = as.mcmc.list(runJagsOut$mcmc)

# combine chains
combined_mcmcfin <- as.data.frame(do.call(rbind, mcmcfin))

### Extract weights -----

# function that determines the index of the parameter
witch <- function(parameter_name){
  which(colnames(mcmcfin[[1]]) == parameter_name)
}

wT_1 <- combined_mcmcfin[, witch("wT[1,1]") : (witch("wT[1,1]") + (SampleSize - 1))]
wT_2 <- combined_mcmcfin[, witch("wT[1,2]") : (witch("wT[1,2]") + (SampleSize - 1))]
wT_3 <- combined_mcmcfin[, witch("wT[1,3]") : (witch("wT[1,3]") + (SampleSize - 1))]

wT_AT_1 <- combined_mcmcfin[, witch("wT_AT[1,1]") : (witch("wT_AT[1,1]") + (SampleSize - 1))]
wT_AT_2 <- combined_mcmcfin[, witch("wT_AT[1,2]") : (witch("wT_AT[1,2]") + (SampleSize - 1))]
wT_AT_3 <- combined_mcmcfin[, witch("wT_AT[1,3]") : (witch("wT_AT[1,3]") + (SampleSize - 1))]

### Calculate subject-level parameter modes

# write function that finds mode
distMode <- function(x){
  as.numeric(names(sort(-table(round(x,3))))[1])
}

participantWeights <- data.frame(
  weight1 = apply(wT_1, 2, distMode),
  weight2 = apply(wT_2, 2, distMode),
  weight3 = apply(wT_3, 2, distMode),
  weight1_AT = apply(wT_AT_1, 2, distMode),
  weight2_AT = apply(wT_AT_2, 2, distMode),
  weight3_AT = apply(wT_AT_3, 2, distMode)
)

rm(wT_1, wT_2, wT_3,
   wT_AT_1, wT_AT_2, wT_AT_3)

# SM Index --------

### Calculate within alternative and within attribute transitions ------

df_subset <- df_subset %>%
  group_by(id_new, session, trial, task) %>%
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

df_subset <- df_subset %>%
  mutate(totalNTransitions = withinAlternative + withinAttribute)

# Add information about number of alternatives and dimensions to data frame

df_subset$NumAlternatives <- 2

df_subset$NumDimensions[df_subset$session == 1] <- 3

if (translation_of_interest == "control") {
  
  df_subset$NumDimensions[df_subset$session == 2] <- 3
  
} else {
  
  df_subset$NumDimensions[df_subset$session == 2] <- 4
  
}


### Calculate SM Index -----

df_subset_exclFixNum1 <- df_subset %>%
  filter(fixNum != 1)

# calculate transitions on trial level
df_subset_exclFixNum1 <- df_subset_exclFixNum1 %>%
  group_by(id_new, trial, task, session) %>%
  summarize(
    withinAlternative = sum(withinAlternative, na.rm = FALSE),
    withinAttribute = sum(withinAttribute, na.rm = FALSE),
    totalNTransitions = sum(totalNTransitions, na.rm = FALSE),
    NumAlternatives = first(NumAlternatives),
    NumDimensions = first(NumDimensions)
  )

# filters trials with no transitions
df_subset_exclFixNum1 <- df_subset_exclFixNum1 %>%
  filter(totalNTransitions != 0)

# calculate trial-wise index
SMIndex <- df_subset_exclFixNum1 %>%
  group_by(id_new, trial, task, session) %>%
  summarize(SM = (sqrt(totalNTransitions) * ((NumAlternatives * NumDimensions/totalNTransitions) * 
                                               (withinAlternative - withinAttribute) - (NumDimensions - NumAlternatives)))/
              (sqrt(NumAlternatives^2 * (NumDimensions - 1) + NumDimensions^2 * (NumAlternatives - 1)))
  )

# calculate participant-wise index
SMIndexAg <- SMIndex %>%
  group_by(id_new, session) %>%
  summarize(meanSM = mean(SM))


# SM Index and Attribute Weights ------

# transform data into long format

participantWeights$id_new <- 1:SampleSize

# add information about SM index to attribute frame

participantWeights_long <- participantWeights %>%
  pivot_longer(
    cols = starts_with("weight"),
    names_to = "var",
    values_to = "value"
  ) %>%
  mutate(
    # determine weight number from the name: "weight1", "weight2", "weight3"
    weight_num = parse_number(var),
    
    # session from whether it ends with "_AT"
    session = if_else(grepl("_AT$", var), 2L, 1L)
  ) %>%
  select(id_new, session, weight_num, value) %>%
  pivot_wider(
    names_from = weight_num,
    values_from = value
  ) %>%
  rename(
    weight1 = `1`,
    weight2 = `2`,
    weight3 = `3`
  ) %>%
  select(weight1, weight2, weight3, session, id_new) %>%
  arrange(id_new, session)

participantWeights_long$session <- as.factor(participantWeights_long$session)

participantWeights_long <- participantWeights_long %>%
  left_join(SMIndexAg,
            by = c("id_new", "session"))

# Plot correlations

plotWeightSM <- function(weightParam, xTitle){
  
  ggplot(data = participantWeights_long,
       aes(x = weightParam,
           y = meanSM,
           color = session)) +
  geom_point() +
  geom_smooth(method='lm') +
  scale_color_manual(values = color_sessions) +
  coord_cartesian(xlim = c(0, 0.85)) +
  labs(x = xTitle,
       y = "Average \nSM Index",
       color = "Session") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12,
                                  face = "bold"),  # style for facet labels
        panel.border = element_rect(color = "black", fill = NA)
  )
  
}

plotW1 <- plotWeightSM(participantWeights_long$weight1,
                       "Weight Price")

plotW2 <- plotWeightSM(participantWeights_long$weight2,
                       "Weight Consumption") +
  theme(axis.title.y = element_blank())

plotW3 <- plotWeightSM(participantWeights_long$weight1,
                       "Weight Popularity") +
  theme(axis.title.y = element_blank())


# Combine plots -----

allWeights <- plotW1 + plotW2 + plotW3 +
  plot_layout(ncol = 3,
              guides = 'collect') &
  theme(plot.margin = margin(4, 4, 4, 4))


# Save plot
filename <- paste0("figures/figureSMIndexAttributeWeights", "_", translation_of_interest, ".png")

ggsave(filename,
       allWeights,
       width = 10,
       height = 4,
       units = "in")




