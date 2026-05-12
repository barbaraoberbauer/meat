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
              "afex",
              "broom.mixed")

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
library(broom.mixed)


rm(package, packages, is_package_installed)


### Load data ---------

load("data/preprocessedDataOriginal.RData")
load("data/preprocessedDataReplication.RData")


# Combine datasets for comparisons ------

# find common columns
common_cols <- intersect(colnames(dfOriginal), colnames(dfReplication))

# subset datasets for these columns
dfOriginal_subset <- dfOriginal[, common_cols]
dfReplication_subset <- dfReplication[, common_cols]

# add information about sample
dfOriginal_subset$sample <- "original"
dfReplication_subset$sample <- "replication"

# combine samples
dfBothSamples <- rbind(dfOriginal_subset, dfReplication_subset)

# turn variable sample into factor
dfBothSamples$sample <- as.factor(dfBothSamples$sample)

# filter for conditions that are equivalent across samples
common_conditions <- c("control", "emissions", "environmental_friendliness",
                           "emission_add", "rating_add")

dfBothSamples <- dfBothSamples %>%
  filter(consumption_translation %in% common_conditions)

# rename conditions
dfBothSamples$consumption_translation[dfBothSamples$consumption_translation == "emissions"] <- 
  "emission_add"

dfBothSamples$consumption_translation[dfBothSamples$consumption_translation == "environmental_friendliness"] <- 
  "rating_add"

# drop unused levels
dfBothSamples$consumption_translation <- droplevels(dfBothSamples$consumption_translation)

# remove unnecessary variables
rm(dfOriginal_subset,
   dfReplication_subset,
   common_conditions,
   common_cols)


# Product Choice -----

### Descriptives  ---------

# function for running descriptives

descriptives_function <- function(data){
  
  # aggregate data on subject level
  choice_prob_id <- data %>%
    group_by(session, consumption_translation, id) %>%
    summarise(mean_choice_id = mean(choice, na.rm = TRUE), 
              se_choice_id = sd(choice, na.rm = TRUE)/sqrt(sum(!is.na(choice)))
    ) %>%
    arrange(consumption_translation)
  
  # aggregate data on group level
  choice_prob_group <- choice_prob_id %>%
    group_by(session, consumption_translation) %>%
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

baseline_choice <- function(data){
  
  # Subset the data for session 1
  data_session1 <- subset(data, session == 1)
  
  # Fit a model without the session interaction
  choice_model_session1 <- glmer(choice ~ consumption_translation + (1 | id) + (1 | task), 
                                 data = data_session1, 
                                 family = binomial(link = "logit"),
                                 control = glmerControl(optimizer="bobyqa", 
                                                        optCtrl = list(maxfun=2e5))
                                 )
  
  return(choice_model_session1)
  
}

baselineChoiceOriginal <- baseline_choice(dfOriginal)
baselineChoiceReplication <- baseline_choice(dfReplication)

# Perform an ANOVA to see if consumption translation has a significant effect
Anova(baselineChoiceOriginal)
Anova(baselineChoiceReplication)


### Check for Significant Fixed Effects -------

fixed_effects_function <- function(data){
  
  # Effect of translation of energy and water consumption (no translation/control) on product choice  
  contrasts(data$consumption_translation) <- 
    contr.treatment(levels(data$consumption_translation), base = 1)
  
  fixed_effects_choice <- afex::mixed(choice ~ (session | id) + (1 | task) + session * consumption_translation, 
                                      data = data, 
                                      family = binomial(link = "logit"), 
                                      control = glmerControl(optimizer="bobyqa", 
                                                             optCtrl = list(maxfun=2e5)),
                                      method = 'LRT')
  
  return(fixed_effects_choice)
  
}


fixedEffectsOriginal <- fixed_effects_function(dfOriginal)
fixedEffectsReplication <- fixed_effects_function(dfReplication)

# also look at significant fixed effects for conditions which are a direct replication
# of the original study (control, emission add, rating add)
fixedEffectsDirectReplication <- fixed_effects_function(dfReplication %>%
                                                          filter(consumption_translation == "control" |
                                                                   consumption_translation == "emission_add" |
                                                                   consumption_translation == "rating_add"))

### Set up model ----

choice_model_function <- function(data){
  
  # Effect of translation of energy and water consumption (no translation/control) on product choice  
  contrasts(data$consumption_translation) <- 
    contr.treatment(levels(data$consumption_translation), base = 1)
  
  choice_model <- glmer(choice ~ (session | id) + (1 | task) + session * consumption_translation, 
                        data = data, 
                        family = binomial(link = "logit"), 
                        control = glmerControl(optimizer="bobyqa", 
                                               optCtrl = list(maxfun=2e5)))
  
  return(choice_model)
  
}

choiceModelOriginal <- choice_model_function(dfOriginal)
choiceModelReplication <- choice_model_function(dfReplication)

summary(choiceModelOriginal)
summary(choiceModelReplication)


### EMM Comparison -------

emm_function <- function(choice_model){
  
  EMM <- emmeans(choice_model, 
                 ~ consumption_translation * session, 
                 type = "response")
  
  emm_session <- pairs(EMM, reverse = TRUE, simple = "session")
  
  emm_session_confint <- confint(emm_session)
  
  return(list(
    emm_session = emm_session,
    emm_session_confint = emm_session_confint
  ))
  
}

emmOriginal <- emm_function(choiceModelOriginal)
emmReplication <- emm_function(choiceModelReplication)


### Compare studies -------

# Baseline model 

baseline_compare_studies <- function(data_combined){
  
  contrasts(data_combined$consumption_translation) <- 
    contr.treatment(levels(data_combined$consumption_translation), base = 1)
  
  data_combined_s1 <- subset(data_combined, session == 1)
  
  baseline_model <- afex::mixed(
    choice ~ consumption_translation * sample + (1 | id) + (1 | task),
    data = data_combined_s1,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa",
                           optCtrl = list(maxfun = 2e5)),
    method = "LRT"
  )
  
  return(baseline_model)
}

baselineChoiceCombined <- baseline_compare_studies(dfBothSamples)


# Fixed effects

fixed_effects_combined_function <- function(data_combined){
  
  contrasts(data_combined$consumption_translation) <- 
    contr.treatment(levels(data_combined$consumption_translation), base = 1)
  
  # Fit combined model
  combined_model <- afex::mixed(
    choice ~ (session | id) + (1 | task) + 
      session * consumption_translation * sample,
    data = data_combined,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa",
                           optCtrl = list(maxfun = 2e5)),
    method = "LRT"
  )
  
  return(combined_model)
  
}

fixedEffectsCombined <- fixed_effects_combined_function(dfBothSamples)

# Perform emmeans comparison

emm_compare_studies <- function(combined_model){
  
  EMM <- emmeans(combined_model,
                 ~ consumption_translation * session * sample,
                 type = "response")
  
  # Get session contrasts (t1 vs t2) for each condition x sample combination
  emm_session <- pairs(EMM, reverse = TRUE, simple = "session")
  
  # Then compare those session contrasts between samples
  emm_interaction <- pairs(emm_session, simple = "sample")
  
  emm_interaction_confint <- confint(emm_interaction)
  
  return(list(
    emm_session = emm_session,
    emm_interaction = emm_interaction,
    emm_interaction_confint = emm_interaction_confint
  ))
  
}

emmCombined <- emm_compare_studies(fixedEffectsCombined)

# Attention -----

### Descriptives  ---------

att_descriptives_function <- function(data){
  
  # aggregate data on subject level
  att_prob_id <- data %>%
    group_by(session, consumption_translation, id) %>%
    summarise(mean_diff_dt_id = mean(diff_t_options, na.rm = TRUE), 
              se_diff_dt_id = sd(diff_t_options, na.rm = TRUE)/sqrt(sum(!is.na(diff_t_options)))
    ) %>%
    arrange(consumption_translation)
  
  # aggregate data on group level
  att_prob_group <- att_prob_id %>%
    group_by(session, consumption_translation) %>%
    summarise(mean_diff_dt = mean(mean_diff_dt_id, na.rm = TRUE), 
              se_diff_dt = sd(mean_diff_dt_id, na.rm = TRUE)/sqrt(sum(!is.na(mean_diff_dt_id)))
    ) %>%
    arrange(consumption_translation)
  
  return(list(
    att_id_level = att_prob_id,
    att_group_level = att_prob_group
  ))
  
}

attDescriptivesOriginal <- att_descriptives_function(dfOriginal)
attDescriptivesReplication <- att_descriptives_function(dfReplication)


### Attention Consistency in Session 1 between Consumption Translations ------

baseline_att <- function(data){
  
  # Subset the data for session 1
  data_session1 <- subset(data, session == 1)
  
  # Fit a model without the session interaction
  att_model_session1 <- lmer(diff_t_options ~ consumption_translation + (1 | id) + (1 | task), 
                             data = data_session1, 
                             REML = FALSE,
                             control = lmerControl(optimizer="bobyqa", 
                                                   optCtrl = list(maxfun=2e5)))
  
  return(att_model_session1)
  
}

baselineAttOriginal <- baseline_att(dfOriginal)
baselineAttReplication <- baseline_att(dfReplication)

# Perform an ANOVA to see if consumption translation has a significant effect
Anova(baselineAttOriginal)
Anova(baselineAttReplication)


### Check for Significant Fixed Effects -------

fixed_effects_att_function <- function(data){
  
  fixed_effects_att <- afex::mixed(diff_t_options ~ (1 | id) + (1 | task) + session * consumption_translation, 
                                   data = data, 
                                   REML = FALSE, 
                                   control = lmerControl(optimizer="bobyqa", 
                                                         optCtrl = list(maxfun=2e5)),
                                   method = 'LRT')
  
  return(fixed_effects_att)
  
}

fixedEffectsAttOriginal <- fixed_effects_att_function(dfOriginal)
fixedEffectsAttReplication <- fixed_effects_att_function(dfReplication)

# also look at significant fixed effects for conditions which are a direct replication
# of the original study (control, emission add, rating add)
fixedEffectsAttDirectReplication <- fixed_effects_att_function(dfReplication %>%
                                                          filter(consumption_translation == "control" |
                                                                   consumption_translation == "emission_add" |
                                                                   consumption_translation == "rating_add"))

### Set up model -------

att_model_function <- function(data){
  
  # Effect of translation of energy and water consumption (no translation/control) on product choice  
  contrasts(data$consumption_translation) <- 
    contr.treatment(levels(data$consumption_translation), base = 1)
  
  attention_model <- lmer(diff_t_options ~ (1 | id) + (1| task) + session * consumption_translation, 
                          data = data, 
                          REML = FALSE, 
                          control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun=2e5)))
  
  return(attention_model)
  
}

attentionModelOriginal <- att_model_function(dfOriginal)
attentionModelReplication <- att_model_function(dfReplication)

summary(attentionModelOriginal)
summary(attentionModelReplication)


### EMM Comparison -------

emm_att_function <- function(attention_model){
  
  EMM_att <- emmeans(attention_model, 
                     ~ consumption_translation * session, 
                     type = "response")
  
  emm_session_att <- pairs(EMM_att, reverse = TRUE, simple = "session")
  
  emm_session_att_confint <- confint(emm_session_att)
  
  # extract estimates
  results_att <- as.data.frame(summary(emm_session_att_confint))[c('consumption_translation',
                                                                   'estimate', 
                                                                   'SE',
                                                                   'asymp.LCL',
                                                                   'asymp.UCL')]
  
  # round to four decimals
  results_att[,c('estimate', 
                 'SE',
                 'asymp.LCL',
                 'asymp.UCL')] <- round(results_att[,c('estimate', 
                                                       'SE',
                                                       'asymp.LCL',
                                                       'asymp.UCL')], 4)
  
  return(list(
    emm_session = emm_session_att,
    emm_session_confint = emm_session_att_confint,
    results_att = results_att
  ))
  
}

emmAttOriginal <- emm_att_function(attentionModelOriginal)
emmAttReplication <- emm_att_function(attentionModelReplication)


# Response Times ----

### Descriptives  ---------

rt_descriptives_function <- function(data){
  
  # aggregate data on subject level
  rt_prob_id <- data %>%
    group_by(session, consumption_translation, id) %>%
    summarise(mean_rt_id = mean(t_decision/1000, na.rm = TRUE), 
              se_rt_id = sd(t_decision/1000, na.rm = TRUE)/sqrt(sum(!is.na(t_decision/1000)))
    ) %>%
    arrange(consumption_translation)
  
  # aggregate data on group level
  rt_prob_group <- rt_prob_id %>%
    group_by(session, consumption_translation) %>%
    summarise(mean_rt = mean(mean_rt_id, na.rm = TRUE), 
              se_rt = sd(mean_rt_id, na.rm = TRUE)/sqrt(sum(!is.na(mean_rt_id)))
    ) %>%
    arrange(consumption_translation)
  
  return(list(
    rt_id_level = rt_prob_id,
    rt_group_level = rt_prob_group
  ))
  
}

rtDescriptivesOriginal <- rt_descriptives_function(dfOriginal)
rtDescriptivesReplication <- rt_descriptives_function(dfReplication)

### RT Consistency in Session 1 between Conditions ------

baseline_rt <- function(data){
  
  # Subset the data for session 1
  data_session1 <- subset(data, session == 1)
  
  # Fit a model without the session interaction
  rt_model_session1 <- glmer(t_decision/1000 ~ consumption_translation + (1 | id) + (1 | task), 
                             data = data_session1, 
                             family = Gamma(link = "log"),
                             control = glmerControl(optimizer="bobyqa", 
                                                    optCtrl = list(maxfun=2e5)))
  
  return(rt_model_session1)
  
}

baselineRtOriginal <- baseline_rt(dfOriginal)
baselineRtReplication <- baseline_rt(dfReplication)

# Perform an ANOVA to see if consumption translation has a significant effect
Anova(baselineRtOriginal)
Anova(baselineRtReplication)


### Check for Significant Fixed Effects -------

fixed_effects_rt_function <- function(data){
  
  contrasts(data$consumption_translation) <- 
    contr.treatment(levels(data$consumption_translation), base = 1)
  
  fixed_effects_rt <- afex::mixed(t_decision/1000 ~ (1 | id) + (1 | task) + session * consumption_translation, 
                                  data = data, 
                                  family = Gamma(link = "log"), 
                                  control = glmerControl(optimizer="bobyqa", 
                                                         optCtrl = list(maxfun=2e5)),
                                  method = 'LRT')
  
  return(fixed_effects_rt)
  
}

fixedEffectsRtOriginal <- fixed_effects_rt_function(dfOriginal)
fixedEffectsRtReplication <- fixed_effects_rt_function(dfReplication)

# Convergence warning for original data may be ignored. Although a small gradient warning persisted, parameter estimates were consistent across multiple optimizers, suggesting the model had practically converged.

# full_model <- glmer(
#   t_decision/1000 ~ (1 | id) + (1 | task) + session * consumption_translation,
#   data = dfOriginal,
#   family = Gamma(link = "log"),
#   control = glmerControl(optimizer = "bobyqa",
#                          optCtrl = list(maxfun = 2e6))
# )
# all_fits <- allFit(full_model)
# summary(all_fits)
# 
# # Compare estimates across optimizers
# all_fits_df <- as.data.frame(summary(all_fits)$fixef)
# print(all_fits_df)


# Singularity warning for the replication study may be ignored, the full model is non singular

# also look at significant fixed effects for conditions which are a direct replication
# of the original study (control, emission add, rating add)
fixedEffectsRtDirectReplication <- fixed_effects_rt_function(dfReplication %>%
                                                          filter(consumption_translation == "control" |
                                                                   consumption_translation == "emission_add" |
                                                                   consumption_translation == "rating_add"))


### Set up model ----

rt_model_function <- function(data){
  
  # Effect of translation of energy and water consumption (no translation/control) on product choice  
  contrasts(data$consumption_translation) <- 
    contr.treatment(levels(data$consumption_translation), base = 1)
  
  rt_model <- glmer(t_decision/1000 ~ (1 | id) + (1 | task) + session * consumption_translation, 
                    data = data, 
                    family = Gamma(link = "log"), 
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun=2e5)))
  
  return(rt_model)
  
}

rtModelOriginal <- rt_model_function(dfOriginal)
rtModelReplication <- rt_model_function(dfReplication)

summary(rtModelOriginal)
summary(rtModelReplication)


### EMM Comparison -------

emm_rt_function <- function(rt_model){
  
  EMM_rt <- emmeans(rt_model, 
                    ~ consumption_translation * session, 
                    type = "response")
  
  emm_session_rt <- pairs(EMM_rt, reverse = TRUE, simple = "session")

  emm_session_rt_confint <- confint(emm_session_rt)

  # extract estimates
  results_rt <- as.data.frame(emm_session_rt_confint)[c('consumption_translation',
                                                        'ratio',
                                                        'SE',
                                                        'asymp.LCL',
                                                        'asymp.UCL')]

  #round to four decimals
  results_rt[,c('ratio',
                'SE',
                'asymp.LCL',
                'asymp.UCL')] <- round(results_rt[,c('ratio',
                                                     'SE',
                                                     'asymp.LCL',
                                                     'asymp.UCL')], 4)

  return(list(
    emm_session = emm_session_rt,
    emm_session_confint = emm_session_rt_confint,
    results_rt = results_rt
  ))
  
}

emmRtOriginal <- emm_rt_function(rtModelOriginal)
emmRtReplication <- emm_rt_function(rtModelReplication)

### Compare studies -------

# Baseline model 

baseline_rt_compare_studies <- function(data_combined){
  
  data_combined_s1 <- subset(data_combined, session == 1)
  
  # rescale rt for convergence
  #data_combined_s1$rt_scaled <- scale(data_combined_s1$t_decision / 1000)[,1]
  
  baseline_model <- afex::mixed(
    t_decision/1000 ~ consumption_translation * sample + (1 | id) + (1 | task),
    data = data_combined_s1,
    family = Gamma(link = "log"),
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e5)),
    method = "LRT"
  )
  
  return(baseline_model)
}

baselineRTCombined <- baseline_rt_compare_studies(dfBothSamples)


# Fixed effects

fixed_effects_rt_combined_function <- function(data_combined){
  
  contrasts(data_combined$consumption_translation) <- 
    contr.treatment(levels(data_combined$consumption_translation), base = 1)
  
  fixed_effects_combined_rt <- afex::mixed(t_decision/1000 ~ (1 | id) + (1 | task) + session * consumption_translation, 
                                  data = data_combined, 
                                  family = Gamma(link = "log"), 
                                  control = glmerControl(optimizer="bobyqa", 
                                                         optCtrl = list(maxfun=2e5)),
                                  method = 'LRT')
  
  return(fixed_effects_combined_rt)
  
}

fixedEffectsCombinedRT <- fixed_effects_rt_combined_function(dfBothSamples)









# Save results -----
behavioralResultsOriginal <- list(att = emmAttOriginal[["results_att"]],
                                  rt = emmRtOriginal[["results_rt"]])

behavioralResultsReplication <- list(att = emmAttReplication[["results_att"]],
                                     rt = emmRtReplication[["results_rt"]])

save(behavioralResultsOriginal, 
     behavioralResultsReplication,
     file = "data/behavioralResults.RData")










