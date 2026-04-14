#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# purpose of script: analysis of behaviorl data
#---

# Reproduce behavioral results by adapting the script from Mertens et al. (2020): https://osf.io/jdep3 
# Only reproduce main effects of consumption translation 
# Adapt script for replication data

# Load packages and read data ------------

#clear working environment
rm(list=ls())

#clear all plots
if(!is.null(dev.list())) dev.off()


### Install packages -------

# List of packages to check and install if necessary
packages <- c("tidyverse",
              "psych",
              "lme4",
              "car",
              "dplyr",
              "lmerTest",
              "emmeans",
              "afex")

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
library(lme4)
library(car)
library(dplyr)
library(lmerTest)
library(emmeans)
library(afex)
library(ggplot2)
library(patchwork)

# Load theme
source("R/theme.R")
theme_set(themeMEAT())


rm(package, packages, is_package_installed)


### Load data ---------

load("data/preprocessedDataOriginal.RData")
load("data/preprocessedDataReplication.RData")

# Product Choice -----

### Descriptives  ---------

# function for running descriptives

descriptives_function <- function(data){
  
  # aggregate data on subject level
  choice_prob_id <- data %>%
    group_by(session, consumption_translation, id, gender) %>%
    summarise(mean_choice_id = mean(choice, na.rm = TRUE), 
              se_choice_id = sd(choice, na.rm = TRUE)/sqrt(sum(!is.na(choice)))
    ) %>%
    arrange(consumption_translation)
  
  # aggregate data on group level
  choice_prob_group <- choice_prob_id %>%
    group_by(session, consumption_translation, gender) %>%
    summarise(mean_choice = mean(mean_choice_id, na.rm = TRUE), 
              se_choice = sd(mean_choice_id, na.rm = TRUE)/sqrt(sum(!is.na(mean_choice_id)))
    ) %>%
    arrange(consumption_translation)
  
  return(list(
    id_level = choice_prob_id,
    group_level = choice_prob_group
  ))
  
}

descriptivesOriginal <- descriptives_function(dfOriginal)
descriptivesReplication <- descriptives_function(dfReplication)

### Choice Consistency in Session 1 between Consumption Translations ------

# exclude one subject that did not indicate gender
dfReplicationGender <- dfReplication %>%
  filter(gender != "Prefer not to say")

dfReplicationGender$gender <- droplevels(dfReplicationGender$gender)

# rename levels to female male in original data
levels(dfOriginal$gender) <- c("Male", "Female")


baseline_choice <- function(data){
  
  # Subset the data for session 1
  data_session1 <- subset(data, session == 1)
  
  # Fit a model without the session interaction
  choice_model_session1 <- glmer(choice ~ gender + consumption_translation + (1 | id) + (1 | task), 
                                 data = data_session1, 
                                 family = binomial(link = "logit"),
                                 control = glmerControl(optimizer="bobyqa", 
                                                        optCtrl = list(maxfun=2e5))
                                 )
  
  return(choice_model_session1)
  
}

baselineChoiceOriginal <- baseline_choice(dfOriginal)
baselineChoiceReplication <- baseline_choice(dfReplicationGender)

# Perform an ANOVA to see if consumption translation has a significant effect
Anova(baselineChoiceOriginal)
Anova(baselineChoiceReplication)


### Check for Significant Fixed Effects -------

fixed_effects_function <- function(data){
  
  fixed_effects_choice <- afex::mixed(choice ~ (session | id) + (1 | task) + session * consumption_translation * gender, 
                                      data = data, 
                                      family = binomial(link = "logit"), 
                                      control = glmerControl(optimizer="bobyqa", 
                                                             optCtrl = list(maxfun=2e5)),
                                      method = 'LRT')
  
  return(fixed_effects_choice)
  
}

fixedEffectsOriginal <- fixed_effects_function(dfOriginal)
fixedEffectsReplication <- fixed_effects_function(dfReplicationGender)


### Set up model ----

choice_model_function <- function(data){
  
  # Effect of translation of energy and water consumption (no translation/control) on product choice  
  contrasts(data$consumption_translation) <- 
    contr.treatment(levels(data$consumption_translation), base = 1)
  
  choice_model <- glmer(choice ~ (session | id) + (1 | task) + gender * session * consumption_translation, 
                        data = data, 
                        family = binomial(link = "logit"), 
                        control = glmerControl(optimizer="bobyqa", 
                                               optCtrl = list(maxfun=2e5)))
  
  return(choice_model)
  
}

choiceModelOriginal <- choice_model_function(dfOriginal)
choiceModelReplication <- choice_model_function(dfReplicationGender)

summary(choiceModelOriginal)
summary(choiceModelReplication)


# Plot gender effects -----

aggregate_function <- function(data){
  
  df_agg_id <- data %>%
    group_by(id, session, gender, consumption_translation) %>%
    summarize(p_choice = mean(choice))
  
  df_agg_id_wide <- df_agg_id %>%
    pivot_wider(names_from = session,
                values_from = p_choice,
                names_prefix = "session_")
  
  df_agg_id_wide$difChoiceProbEco <- df_agg_id_wide$session_2 - df_agg_id_wide$session_1
  
  df_agg <- df_agg_id %>%
    group_by(session, gender, consumption_translation) %>%
    summarize(p_choice = mean(p_choice))
  
  return(list(
    df_agg_id = df_agg_id,
    df_agg_id_wide = df_agg_id_wide,
    df_agg = df_agg
  )
  )
  
}

aggOriginal <- aggregate_function(dfOriginal)
aggReplication <- aggregate_function(dfReplicationGender)

plotGenderDifferences <- function(data, labels, title){
  
  ggplot(data = data,
         aes(x = consumption_translation,
             y = difChoiceProbEco,
             fill = gender)) +
    geom_hline(yintercept = 0,
               linetype = "dashed",
               linewidth = 1) +
    geom_point(aes(color = gender),
               position = position_jitterdodge(dodge.width = 0.6,
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
    scale_fill_manual(values = scales::alpha(color_gender, 0.4)) +  
    scale_color_manual(values = color_gender) +
    scale_x_discrete(labels = labels) +
    coord_cartesian(ylim = c(-1, 1)) +
    labs(x = "Experimental Condition",
         y = "Differential Probability of Choosing Eco \n(Session 2 - Session 1)",
         fill = "Gender",
         title = title)
  
}


genderDifferencesOriginal <-
  plotGenderDifferences(aggOriginal[["df_agg_id_wide"]],
                        labelsOriginal,
                        "Original")

genderDifferencesReplication <-
  plotGenderDifferences(aggReplication[["df_agg_id_wide"]],
                        labelsReplication,
                        "Replication")

# Combine plots

plotGenderDifferences <-
  genderDifferencesOriginal +
  genderDifferencesReplication +
  plot_layout(guides = 'collect',
              axis_title = 'collect') &
  theme(legend.position = 'top')


# Save plot
ggsave("figures/figureGenderDifferences.pdf",
       plotGenderDifferences,
       width = 12,
       height = 6,
       units = "in",
       device = cairo_pdf)


