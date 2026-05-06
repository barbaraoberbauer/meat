#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# purpose: analyze relationship between dwell time and choice
#---

# Process tracing analysis

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


rm(package, packages, is_package_installed)


### Load data ---------

load("data/preprocessedDataOriginal.RData")
load("data/preprocessedDataReplication.RData")

# Calculate dwell time differences --------

dfOriginal$ddt_eco_noneco <- (dfOriginal$t_option1 - dfOriginal$t_option0)/1000 # in sec
dfOriginal$ddt_eco_noneco_norm <- (dfOriginal$t_option1 - dfOriginal$t_option0)/(dfOriginal$t_option1 + dfOriginal$t_option0)
dfOriginal$ddt_scaled <- scale(dfOriginal$ddt_eco_noneco_norm)

dfReplication$ddt_eco_noneco <- (dfReplication$t_option1 - dfReplication$t_option0)/1000 # in sec
dfReplication$ddt_eco_noneco_norm <- (dfReplication$t_option1 - dfReplication$t_option0)/(dfReplication$t_option1 + dfReplication$t_option0)
dfReplication$ddt_scaled <- scale(dfReplication$ddt_eco_noneco_norm)


# Influence of option attention on choice ----

# Following Yang & Krajbich, https://supp.apa.org/psycarticles/supplemental/rev0000350/rev0000350_sm.pdf

# test whether the relationship between dwell time (advantage on ecological option) and choice differs
# across conditions and sessions

### Check for Significant Fixed Effects -------

fixed_effects_function <- function(data){
  
  fixed_effects_choice <- afex::mixed(choice ~ (1 | id) + session * ddt_scaled *
                                        consumption_translation, 
                                      data = data, 
                                      family = binomial(link = "logit"), 
                                      control = glmerControl(optimizer="bobyqa", 
                                                             optCtrl = list(maxfun=2e5)),
                                      method = 'LRT')
  
  return(fixed_effects_choice)
  
}

fixedEffectsOriginal <- fixed_effects_function(dfOriginal)
fixedEffectsReplication <- fixed_effects_function(dfReplication)

### Set up model ----

choice_dwellTime_model_function <- function(data){
  
  # Effect of translation of energy and water consumption (no translation/control) on product choice  
  contrasts(data$consumption_translation) <- 
    contr.treatment(levels(data$consumption_translation), base = 1)
  
  model <- glmer(choice ~ (1 | id) + session * ddt_scaled *
                   consumption_translation,, 
                 data = data, 
                 family = binomial(link = "logit"), 
                 control = glmerControl(optimizer="bobyqa", 
                                        optCtrl = list(maxfun=2e5)))
  
  return(model)
  
}

dwellTimeChoiceOriginal <- choice_dwellTime_model_function(dfOriginal)
dwellTimeChoiceReplication <- choice_dwellTime_model_function(dfReplication)

summary(dwellTimeChoiceOriginal)
summary(dwellTimeChoiceReplication)


### EMM Comparison -------

emtrend_function <- function(choice_model) {
  
  trends <- emtrends(choice_model,
                     ~ session * consumption_translation,
                     var = "ddt_scaled")
  
  # Compare sessions only within each consumption_translation level
  trend_session_pairs <- pairs(trends, 
                               simple = "session",
                               reverse = TRUE)
  
  confint_session_pairs <- confint(trend_session_pairs)
  
  return(list(
    trends                = trends,
    trend_session_pairs   = trend_session_pairs,
    confint_session_pairs = confint_session_pairs
  ))
}

trendsOriginal    <- emtrend_function(dwellTimeChoiceOriginal)
trendsReplication <- emtrend_function(dwellTimeChoiceReplication)

# Calculate dwell time on consumption --------

dfOriginal$ddt_consumption <- rowSums(dfOriginal[,c("t_consumption0",
                                                    "t_consumption1",
                                                    "t_consumption_translation0",
                                                    "t_consumption_translation1")],
                                      na.rm = TRUE)

dfOriginal$ddt_consumption_norm <- dfOriginal$ddt_consumption/(dfOriginal$t_option1 + dfOriginal$t_option0)
dfOriginal$ddt_consumption_scaled <- scale(dfOriginal$ddt_consumption_norm)
dfOriginal$ddt_consumption <- dfOriginal$ddt_consumption/1000 # in sec


dfReplication$ddt_consumption <- rowSums(dfReplication[,c("t_consumption0",
                                                          "t_consumption1",
                                                          "t_consumption_translation0",
                                                          "t_consumption_translation1")],
                                         na.rm = TRUE)

dfReplication$ddt_consumption_norm <- dfReplication$ddt_consumption/(dfReplication$t_option1 + dfReplication$t_option0)
dfReplication$ddt_consumption_scaled <- scale(dfReplication$ddt_consumption_norm)
dfReplication$ddt_consumption <- dfReplication$ddt_consumption/1000 # in sec


# Influence of consumption attention on choice --------

### Check for Significant Fixed Effects -------

fixed_effects_consumption <- function(data){
  
  fixed_effects_choice <- afex::mixed(choice ~ (1 | id) + session * ddt_consumption_scaled *
                                        consumption_translation, 
                                      data = data, 
                                      family = binomial(link = "logit"), 
                                      control = glmerControl(optimizer="bobyqa", 
                                                             optCtrl = list(maxfun=2e5)),
                                      method = 'LRT')
  
  return(fixed_effects_choice)
  
}

fixedEffectsConsumptionOriginal <- fixed_effects_consumption(dfOriginal)
fixedEffectsConsumptionReplication <- fixed_effects_consumption(dfReplication)

### Set up model ----

choice_dwellTimeConsumption_model_function <- function(data){
  
  # Effect of translation of energy and water consumption (no translation/control) on product choice  
  contrasts(data$consumption_translation) <- 
    contr.treatment(levels(data$consumption_translation), base = 1)
  
  model <- glmer(choice ~ (1 | id) + session * ddt_consumption_scaled *
                   consumption_translation,, 
                 data = data, 
                 family = binomial(link = "logit"), 
                 control = glmerControl(optimizer="bobyqa", 
                                        optCtrl = list(maxfun=2e5)))
  
  return(model)
  
}

dwellTimeConsumptionChoiceOriginal <- choice_dwellTimeConsumption_model_function(dfOriginal)
dwellTimeConsumptionChoiceReplication <- choice_dwellTimeConsumption_model_function(dfReplication)

summary(dwellTimeConsumptionChoiceOriginal)
summary(dwellTimeConsumptionChoiceReplication)

### EMM Comparison -------

emtrendConsumption_function <- function(choice_model) {
  
  trends <- emtrends(choice_model,
                     ~ session * consumption_translation,
                     var = "ddt_consumption_scaled")
  
  # Compare sessions only within each consumption_translation level
  trend_session_pairs <- pairs(trends, 
                               simple = "session",
                               reverse = TRUE)
  
  confint_session_pairs <- confint(trend_session_pairs)
  
  return(list(
    trends                = trends,
    trend_session_pairs   = trend_session_pairs,
    confint_session_pairs = confint_session_pairs
  ))
}

trendsConsumptionOriginal    <- emtrendConsumption_function(dwellTimeConsumptionChoiceOriginal)
trendsConsumptionReplication <- emtrendConsumption_function(dwellTimeConsumptionChoiceReplication)

