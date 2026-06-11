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
              "ggpubr",
              "cocor")

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
library(cocor)

# Load theme
source("R/theme.R")
theme_set(themeMEAT())

rm(package, packages, is_package_installed)


### Load data ---------

load("data/preprocessedDataReplication.RData")

### Specify conditions -------

# specify modeling data
dataset <- "replication"

# get translations
conditions <- levels(dfReplication$consumption_translation)

# set data sets to be used
times <- list(control = "20260520_1545",
              emission_replace = "20260522_2028",
              rating_replace = "20260522_1441",
              emission_add = "20260521_1608",
              rating_add = "20260518_2319")


### Set up functions -------

# function that determines the index of the parameter
witch <- function(parameter_name){
  which(colnames(mcmcfin[[1]]) == parameter_name)
}

# write function that finds mode
distMode <- function(x){
  as.numeric(names(sort(-table(round(x,3))))[1])
}

# Create data frames with weights and SM index ---------

# Loop over conditions 

SMIndexAttributeWeights <- list()

for (con in seq_along(conditions)) {
  
  # set translation of interest
  translation_of_interest <- conditions[con]
  
  # set time
  time <- times[conditions[con]]
  
  # progress statement
  cat(sprintf("Processing condition %d / %d: %s\n", con, length(conditions), translation_of_interest))
  
  # generate filename
  filename <- paste0("data/modeling/runJagsOutmaaDDMDirichlet", "_", 
                     dataset, "_", 
                     translation_of_interest, "_", 
                     time, ".rds")
  
  runJagsOut <- readRDS(filename)
  
  #+++++++++++++++++++++
  # set sample
  
  df_subset <- dfReplicationProcess %>%
    filter(consumption_translation == translation_of_interest)
  
  # assign new ids that are starting from 1 and increment by 1
  df_subset <- df_subset %>%
    mutate(id_new = dense_rank(id))
  
  # sample size
  SampleSize <- length(unique(df_subset$id_new))
  
  #+++++++++++++++++++++
  # derive attribute weights
  
  # store as mcmc object
  mcmcfin = as.mcmc.list(runJagsOut$mcmc)
  
  # combine chains
  combined_mcmcfin <- as.data.frame(do.call(rbind, mcmcfin))
  
  # extract weights
  wT_1 <- combined_mcmcfin[, witch("wT[1,1]") : (witch("wT[1,1]") + (SampleSize - 1))]
  wT_2 <- combined_mcmcfin[, witch("wT[1,2]") : (witch("wT[1,2]") + (SampleSize - 1))]
  wT_3 <- combined_mcmcfin[, witch("wT[1,3]") : (witch("wT[1,3]") + (SampleSize - 1))]
  
  wT_AT_1 <- combined_mcmcfin[, witch("wT_AT[1,1]") : (witch("wT_AT[1,1]") + (SampleSize - 1))]
  wT_AT_2 <- combined_mcmcfin[, witch("wT_AT[1,2]") : (witch("wT_AT[1,2]") + (SampleSize - 1))]
  wT_AT_3 <- combined_mcmcfin[, witch("wT_AT[1,3]") : (witch("wT_AT[1,3]") + (SampleSize - 1))]
  
  #+++++++++++++++++++++
  # calculate subject-level parameter modes
  
  participantWeights <- data.frame(
    weight1 = apply(wT_1, 2, distMode),
    weight2 = apply(wT_2, 2, distMode),
    weight3 = apply(wT_3, 2, distMode),
    weight1_AT = apply(wT_AT_1, 2, distMode),
    weight2_AT = apply(wT_AT_2, 2, distMode),
    weight3_AT = apply(wT_AT_3, 2, distMode)
  )
  
  #+++++++++++++++++++++
  # SM index
  
  # Calculate within alternative and within attribute transitions 
  
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
  
  # Calculate SM Index
  
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
                                                 (withinAlternative - withinAttribute) - 
                                                 (NumDimensions - NumAlternatives)))/
                (sqrt(NumAlternatives^2 * (NumDimensions - 1) + 
                        NumDimensions^2 * (NumAlternatives - 1)))
    )
  
  # calculate participant-wise index
  SMIndexAg <- SMIndex %>%
    group_by(id_new, session) %>%
    summarize(meanSM = mean(SM))
  
  #+++++++++++++++++++++
  # SM index and attribute weights
  
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
  
  #+++++++++++++++++++++
  # store resulting df
  
  SMIndexAttributeWeights[translation_of_interest] <- list(participantWeights_long)
  
  
}

# Calculate correlations ------

correlationResults <- list()

for (con in seq_along(conditions)) {
  
  # set translation of interest
  translation_of_interest <- conditions[con]
  
  # set data
  df_subset <- SMIndexAttributeWeights[[translation_of_interest]]
  
  # transform to wide format
  df_subset_wide <- df_subset %>%
    pivot_wider(
      id_cols = id_new,
      names_from = session,
      values_from = c(weight1, weight2, weight3, meanSM)
    )
  
  # omit NA
  df_subset_wide <- na.omit(df_subset_wide)
  
  # calculate correlation differences
  results_cocor <- list()
  
  for (weight in c("weight1", "weight2", "weight3")) {
    
    r.jk <- cor(df_subset_wide[[paste0(weight, "_1")]], df_subset_wide$meanSM_1)
    r.hm <- cor(df_subset_wide[[paste0(weight, "_2")]], df_subset_wide$meanSM_2)
    r.jh <- cor(df_subset_wide[[paste0(weight, "_1")]], df_subset_wide[[paste0(weight, "_2")]])
    r.jm <- cor(df_subset_wide[[paste0(weight, "_1")]], df_subset_wide$meanSM_2)
    r.kh <- cor(df_subset_wide$meanSM_1, df_subset_wide[[paste0(weight, "_2")]])
    r.km <- cor(df_subset_wide$meanSM_1, df_subset_wide$meanSM_2)
    
    results_cocor[[weight]] <- cocor.dep.groups.nonoverlap(
      r.jk = r.jk, r.hm = r.hm,
      r.jh = r.jh, r.jm = r.jm,
      r.kh = r.kh, r.km = r.km,
      n = nrow(testDf_wide)
    )
  }
  
  correlationResults[[translation_of_interest]] <- results_cocor
  
}

# Plot correlations ---------

# function for creating plots
plotWeightSM <- function(df, weightParam, xTitle){
  
  ggplot(data = df,
       aes(x = .data[[weightParam]],
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

# Loop across conditions

allPlots <- list()
for (con in seq_along(conditions)) {
  
  # set translation of interest
  translation_of_interest <- conditions[con]
  
  # extract and clean column names
  df_con <- as.data.frame(SMIndexAttributeWeights[translation_of_interest])
  names(df_con) <- sub(paste0("^", translation_of_interest, "\\."), "", names(df_con))
  
  # weight1
  plotPrice <- plotWeightSM(df_con, "weight1", "Weight Price")
  
  # weight2
  plotConsumption <- plotWeightSM(df_con, "weight2", "Weight Consumption")
  
  # weight3
  plotPopularity <- plotWeightSM(df_con, "weight3", "Weight Popularity")
  
  allPlots[[translation_of_interest]] <- list(plotPrice       = plotPrice,
                                              plotConsumption = plotConsumption,
                                              plotPopularity  = plotPopularity)
}

# Combine plots

no_y <- theme(axis.title.y = element_blank(), axis.text.y = element_blank(),
              axis.ticks.y = element_blank())

no_x <- theme(axis.title.x = element_blank())

conditionNames <- c("Control", "Carbon\n Emissions\n Replace", "Rating\n Replace", "Carbon\nEmission\n Add", "Rating\n Add")

finalPlot <- (
  # Row 1: Weight Price
  allPlots[["control"]]$plotPrice + no_x + ggtitle(conditionNames[1]) |
    (allPlots[["emission_replace"]]$plotPrice + no_y + no_x + ggtitle(conditionNames[2])) |
    (allPlots[["rating_replace"]]$plotPrice + no_y + ggtitle(conditionNames[3])) |
    (allPlots[["emission_add"]]$plotPrice + no_y + no_x + ggtitle(conditionNames[4])) |
    (allPlots[["rating_add"]]$plotPrice + no_y + no_x + ggtitle(conditionNames[5]))
) / (
  # Row 2: Weight Consumption
  allPlots[["control"]]$plotConsumption + no_x |
    (allPlots[["emission_replace"]]$plotConsumption + no_y + no_x) |
    (allPlots[["rating_replace"]]$plotConsumption + no_y) |
    (allPlots[["emission_add"]]$plotConsumption + no_y + no_x) |
    (allPlots[["rating_add"]]$plotConsumption + no_y + no_x)
) / (
  # Row 3: Weight Popularity
  allPlots[["control"]]$plotPopularity + no_x |
    (allPlots[["emission_replace"]]$plotPopularity + no_x + no_y) |
    (allPlots[["rating_replace"]]$plotPopularity + no_y) |
    (allPlots[["emission_add"]]$plotPopularity + no_x + no_y) |
    (allPlots[["rating_add"]]$plotPopularity + no_x + no_y)
) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        plot.margin = margin(4, 4, 4, 4),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12))


# Save plot
ggsave("figures/figureSMIndexAttributeWeights.png",
       finalPlot,
       width = 10,
       height = 8,
       units = "in")

save(finalPlot,
     file = "figures/figureSMIndexAttributeWeights.RData")




