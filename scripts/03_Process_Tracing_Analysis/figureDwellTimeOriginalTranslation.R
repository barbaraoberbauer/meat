#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# purpose: plot dwell time proportions between original attribute and translations
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

load("data/preprocessedDataOriginal.RData")
load("data/preprocessedDataReplication.RData")


# Calculate proportional dwell time between original attribute and translation + change in choice probability ------

createDf <- function(df){
  
  # subset session 2
  df_subset <- df %>% filter(session == 2)
  
  # calculate average dwell time on original attribute and translation
  df_subset$t_consumption_all <- 
    (df_subset$t_consumption0 + df_subset$t_consumption1)/2
  df_subset$t_consumption_translation_all <- 
    (df_subset$t_consumption_translation0 + df_subset$t_consumption_translation1)/2
  
  # calculate ratio 
  df_subset$t_ratio_original_translation <- 
    (df_subset$t_consumption_all + 1)/
    (df_subset$t_consumption_all + df_subset$t_consumption_translation_all + 1)
  
  # aggregate ratio
  dtRatioOriginalTranslation <- df_subset %>%
    group_by(consumption_translation, id) %>%
    summarize(averageRatio = mean(t_ratio_original_translation))
  
  # calculate change in eco choice 
  
  choiceProbabilities <- df %>%
    group_by(id, consumption_translation, session) %>%
    summarize(choiceProbability = mean(choice))
  
  choiceProbabilitiesWide <- choiceProbabilities %>%
    pivot_wider(names_from = session,
                values_from = choiceProbability,
                names_prefix = "session_")
  
  choiceProbabilitiesWide$choiceProbDifference <- 
    choiceProbabilitiesWide$session_2 -
    choiceProbabilitiesWide$session_1
  
  # add to dtRatioOriginalTranslation
  dtRatioOriginalTranslation <- dtRatioOriginalTranslation %>%
    left_join(choiceProbabilitiesWide,
              by = c("id", "consumption_translation")) 
  
  return(dtRatioOriginalTranslation)
  
}

# filter relevant conditions 
dfOriginalRelevantConditions <- dfOriginal %>%
  filter(consumption_translation != "control")

dfReplicationRelevantConditions <- dfReplication %>%
  filter(consumption_translation == "emission_add" |
           consumption_translation == "rating_add")

# calculate dtRatios
dtRatioOriginalTranslationOriginal <- createDf(dfOriginalRelevantConditions) 

dtRatioOriginalTranslationReplication <- createDf(dfReplicationRelevantConditions) 



# Plot relationship between dwell times and choice ----

plotDtChoice <- function(dfRatio, labels, title){
  
  ggplot(data = dfRatio,
         aes(x = choiceProbDifference,
             y = averageRatio)) +
    geom_point() +
    labs(x = "Change in Ecological Choice Probability \n(Session 2 - Session 1)" , 
         y = "Proportional Dwell Time \non Original Attribute",
         title = title) +
    coord_cartesian(ylim = c(0, 1.2)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.25),
                       limits = NULL) +
    geom_smooth(method='lm') +
    facet_grid(~consumption_translation,
               labeller = labeller(consumption_translation = labels)) +
    stat_cor(method = "spearman",
             cor.coef.name = "rho",
             p.digits = 2,
             label.y = 1.15,
             size = 4) +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 12,
                                    face = "bold"),  # style for facet labels
          panel.border = element_rect(color = "black", 
                                      fill = NA),
          panel.spacing = unit(0.7, "cm")
    )
  
  
}

plotDtChoiceOriginal <- 
  plotDtChoice(dtRatioOriginalTranslationOriginal,
               labelsOriginal,
               "Study 1")

plotDtChoiceReplication <- 
  plotDtChoice(dtRatioOriginalTranslationReplication,
               labelsReplication,
               "Study 2")

# Plot proportional dwell times ------

# Combine data

dtRatioOriginalTranslationOriginal$sample <- "original"
dtRatioOriginalTranslationReplication$sample <- "replication"

dtRatioOriginalTranslationReplication$id <- as.character(dtRatioOriginalTranslationReplication$id)

dtRatioOriginalTranslationAll <- 
  rbind(dtRatioOriginalTranslationOriginal, 
    dtRatioOriginalTranslationReplication)

dtRatioOriginalTranslationAll$sample <-
  as.factor(dtRatioOriginalTranslationAll$sample)

# rename conditions
dtRatioOriginalTranslationAll$consumption_translation[dtRatioOriginalTranslationAll$consumption_translation == "emission_add"] <- 
  "emissions"

dtRatioOriginalTranslationAll$consumption_translation[dtRatioOriginalTranslationAll$consumption_translation == "rating_add"] <- 
  "environmental_friendliness"

plotRatioDt <-
ggplot(data = dtRatioOriginalTranslationAll,
       aes(x = consumption_translation,
           y = averageRatio,
           color = sample)) +
  geom_hline(yintercept = 0.5,
             linetype = "dashed",
             linewidth = 1) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.5, 
                                             jitter.width = 0.2, 
                                             jitter.height = 0),
             size = 1.1, alpha = 0.3) +  # reduce alpha further
  stat_summary(fun.data = mean_se,
               aes(group = sample),
               geom = "errorbar",
               position = position_dodge(width = 0.5),
               width = 0.2,
               linewidth = 1.25,
               color = "black") +
  stat_summary(fun = mean, 
               aes(group = sample),
               geom = "point",
               position = position_dodge(width = 0.5),
               size = 2,
               color = "black") +  # larger so it stands out
  scale_x_discrete(labels = labelsOriginal) +
  scale_color_manual(values = c("original" = "#ABABAB", 
                                "replication" = "#333333"),
                     labels = c("original" = "1", 
                                "replication" = "2")) +
  labs(x = "Experimental Condition", 
       y = "Proportional Dwell Time \n on Original Attribute",
       color = "Study") +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  theme(legend.position="bottom")

# Combine and save plots -----

allPlots <- plotDtChoiceOriginal/
  (plotDtChoiceReplication + plotRatioDt) +
  plot_annotation(
    tag_levels = list(c("a", "b", "c"))
  ) &
  theme(
    plot.tag = element_text(size = 20, face = "bold")
  )

ggsave("figures/figureDTOriginalTranslation.png", 
       allPlots, 
       width = 10,
       height = 10,
       units = "in")




