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
              "afex",
              "FSA")

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
library(FSA)


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

### Compare studies -----

combined_fixed_effects_function <- function(data_combined){
  
  fixed_effects_choice <- afex::mixed(choice ~ (1 | id) + session * ddt_scaled *
                                        consumption_translation * sample, 
                                      data = data_combined, 
                                      family = binomial(link = "logit"), 
                                      control = glmerControl(optimizer="bobyqa", 
                                                             optCtrl = list(maxfun=2e5)),
                                      method = 'LRT')
  
  return(fixed_effects_choice)
  
}

fixedEffectsCombined <- combined_fixed_effects_function(dfBothSamples)

# check marginal means 

emm_compare_studies_slopes <- function(combined_model){
  
  slopes <- emtrends(combined_model,
                     ~ sample,
                     var = "ddt_scaled")
  
  # Compare slopes between samples
  slopes_comparison <- pairs(slopes, reverse = TRUE)
  slopes_confint <- confint(slopes_comparison)
  
  return(list(
    slopes = slopes,
    slopes_comparison = slopes_comparison,
    slopes_confint = slopes_confint
  ))
}

emmSlopesCombined <- emm_compare_studies_slopes(fixedEffectsCombined)



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

### Compare studies -----

fixed_effects_consumption_combined <- function(data_combined){
  
  fixed_effects_choice <- afex::mixed(choice ~ (1 | id) + session * ddt_consumption_scaled *
                                        consumption_translation * sample, 
                                      data = data_combined, 
                                      family = binomial(link = "logit"), 
                                      control = glmerControl(optimizer="bobyqa", 
                                                             optCtrl = list(maxfun=2e5)),
                                      method = 'LRT')
  
  return(fixed_effects_choice)
  
}

fixedEffectsConsumptionCombined <- fixed_effects_consumption_combined(dfBothSamples)

# check marginal means 

emm_compare_studies_slopes_consumption <- function(combined_model){
  
  slopes <- emtrends(combined_model,
                     ~ sample,
                     var = "ddt_consumption_scaled")
  
  # Compare slopes between samples
  slopes_comparison <- pairs(slopes, reverse = TRUE)
  slopes_confint <- confint(slopes_comparison)
  
  return(list(
    slopes = slopes,
    slopes_comparison = slopes_comparison,
    slopes_confint = slopes_confint
  ))
}

emmSlopesCombinedConsumption <- emm_compare_studies_slopes_consumption(fixedEffectsConsumptionCombined)



# Calculate proportional dwell time differences ------

### Calculate dwell time proportions ------

calculate_fix_props <- function(dat, dataset){
  
  fixProps <- data.frame(price_0 = rep(NA, nrow(dat)),
                         consumption_0 = rep(NA, nrow(dat)),
                         popularity_0 = rep(NA, nrow(dat)),
                         price_1 = rep(NA, nrow(dat)),
                         consumption_1 = rep(NA, nrow(dat)),
                         popularity_1 = rep(NA, nrow(dat))) 
  
  # attributes and their translation are treated as one attribute for simplicity
  # depending on dataset, summarize price and price translation
  if (dataset == "original") {
    
    fixProps$price_0 <- rowSums(dat[, c("t_price0", "t_price_translation0")], na.rm = TRUE)/1000
    fixProps$price_1 <- rowSums(dat[, c("t_price1", "t_price_translation1")], na.rm = TRUE)/1000
    
  } else if (dataset == "replication") {
    
    fixProps$price_0 <- dat$t_price0/1000
    fixProps$price_1 <- dat$t_price1/1000
    
  }
  
  fixProps$consumption_0 <- rowSums(dat[, c("t_consumption0", "t_consumption_translation0")], na.rm = TRUE)/1000
  fixProps$popularity_0 <- dat$t_popularity0/1000
  fixProps$consumption_1 <- rowSums(dat[, c("t_consumption1", "t_consumption_translation1")], na.rm = TRUE)/1000
  fixProps$popularity_1 <- dat$t_popularity1/1000
  
  # divide by total duration of the trial
  #fixProps <- fixProps/abs(df_subset$t_decision) #take absolute value instead of +/- coded RT
  fixProps <- fixProps/dat$t_total # divide by total dwell time
  
  # normalize each trial to 1
  fixProps <- fixProps/rowSums(fixProps)
  
  return(fixProps)
  
}

fixPropsOriginal <- calculate_fix_props(dfOriginal, "original")
fixPropsReplication <- calculate_fix_props(dfReplication, "replication")

# add dwell time proportions to data frames
fixPropsOriginal <- cbind(fixPropsOriginal, dfOriginal[, c("task", 
                                                           "id", 
                                                           "session", 
                                                           "consumption_translation")])
fixPropsReplication <- cbind(fixPropsReplication, dfReplication[, c("task", 
                                                                    "id", 
                                                                    "session", 
                                                                    "consumption_translation")])

# calculate net dwell time on attributes

fixPropsOriginal$price_all <- (fixPropsOriginal$price_0 + fixPropsOriginal$price_1)/2
fixPropsOriginal$consumption_all <- (fixPropsOriginal$consumption_0 + fixPropsOriginal$consumption_1)/2
fixPropsOriginal$popularity_all <- (fixPropsOriginal$popularity_0 + fixPropsOriginal$popularity_1)/2

fixPropsReplication$price_all <- (fixPropsReplication$price_0 + fixPropsReplication$price_1)/2
fixPropsReplication$consumption_all <- (fixPropsReplication$consumption_0 + fixPropsReplication$consumption_1)/2
fixPropsReplication$popularity_all <- (fixPropsReplication$popularity_0 + fixPropsReplication$popularity_1)/2


### Fit generalized linear mixed models, separately for all attributes -------

fixed_effects_proportional_dwelltimes <- function(dat){
  
  # use control as reference level
  contrasts(dat$consumption_translation) <- 
    contr.treatment(levels(dat$consumption_translation), base = 1)

  fixed_effects_price <- afex::mixed(price_all ~ (1 | task) + (1 | id) + consumption_translation * session,
                                      data = dat,
                                      control = lmerControl(optimizer="bobyqa",
                                                             optCtrl = list(maxfun=2e5)),
                                      method = 'LRT')
  
  fixed_effects_consumption <- afex::mixed(consumption_all ~ (1 | task) + (1 | id) + consumption_translation * session,
                                     data = dat,
                                     control = lmerControl(optimizer="bobyqa",
                                                           optCtrl = list(maxfun=2e5)),
                                     method = 'LRT')
  
  fixed_effects_popularity <- afex::mixed(popularity_all ~ (1 | task) + (1 | id) + consumption_translation * session,
                                     data = dat,
                                     control = lmerControl(optimizer="bobyqa",
                                                           optCtrl = list(maxfun=2e5)),
                                     method = 'LRT')
  

  return(list(fixed_effects_price = fixed_effects_price,
              fixed_effects_consumption = fixed_effects_consumption,
              fixed_effects_popularity = fixed_effects_popularity))

}

fixedEffectsProportionalDTOriginal <- fixed_effects_proportional_dwelltimes(fixPropsOriginal)
fixedEffectsProportionalDTReplication <- fixed_effects_proportional_dwelltimes(fixPropsReplication)


### Calculate marginal means -------

emm_proportional_dt <- function(fixed_effects_list, choice_model){
  
  # Price
  emm_price <- emmeans(fixed_effects_list$fixed_effects_price, 
                       ~ consumption_translation * session)
  emm_price_session <- pairs(emm_price, reverse = TRUE, simple = "session")
  emm_price_vs_control <- contrast(emm_price,
                                   interaction = list(
                                     session = "consec",
                                     consumption_translation = "trt.vs.ctrl"
                                   ))
  
  # Consumption
  emm_consumption <- emmeans(fixed_effects_list$fixed_effects_consumption,
                             ~ consumption_translation * session)
  emm_consumption_session <- pairs(emm_consumption, reverse = TRUE, simple = "session")
  emm_consumption_vs_control <- contrast(emm_consumption,
                                         interaction = list(
                                           session = "consec",
                                           consumption_translation = "trt.vs.ctrl"
                                         ))
  
  # Popularity
  emm_popularity <- emmeans(fixed_effects_list$fixed_effects_popularity,
                            ~ consumption_translation * session)
  emm_popularity_session <- pairs(emm_popularity, reverse = TRUE, simple = "session")
  emm_popularity_vs_control <- contrast(emm_popularity,
                                        interaction = list(
                                          consumption_translation = "trt.vs.ctrl",
                                          session = "consec"
                                        ))
  
  return(list(
    price = list(emm = emm_price, 
                 session = emm_price_session,
                 vs_control = emm_price_vs_control,
                 confint = confint(emm_price_session),
                 vs_control_conf = confint(emm_price_vs_control)),
    consumption = list(emm = emm_consumption,
                       session = emm_consumption_session,
                       vs_control = emm_consumption_vs_control,
                       confint = confint(emm_consumption_session),
                       vs_control_conf = confint(emm_consumption_vs_control)),
    popularity = list(emm = emm_popularity,
                      session = emm_popularity_session,
                      vs_control = emm_popularity_vs_control,
                      confint = confint(emm_popularity_session),
                      vs_control_conf = (confint(emm_popularity_vs_control)))
  ))
}

emmProportionalDTOriginal <- emm_proportional_dt(fixedEffectsProportionalDTOriginal)
emmProportionalDTReplication <- emm_proportional_dt(fixedEffectsProportionalDTReplication)


### Combine datasets for dwell times  -------

# find common columns
common_cols <- intersect(colnames(fixPropsOriginal), colnames(fixPropsReplication))

# subset datasets for these columns
fixPropsOriginal_subset <- fixPropsOriginal[, common_cols]
fixPropsReplication_subset <- fixPropsReplication[, common_cols]

# add information about sample
fixPropsOriginal_subset$sample <- "original"
fixPropsReplication_subset$sample <- "replication"

# combine samples
fixPropsBothSamples <- rbind(fixPropsOriginal_subset, fixPropsReplication_subset)

# turn variable sample into factor
fixPropsBothSamples$sample <- as.factor(fixPropsBothSamples$sample)

# filter for conditions that are equivalent across samples
common_conditions <- c("control", "emissions", "environmental_friendliness",
                       "emission_add", "rating_add")

fixPropsBothSamples <- fixPropsBothSamples %>%
  filter(consumption_translation %in% common_conditions)

# rename conditions
fixPropsBothSamples$consumption_translation[fixPropsBothSamples$consumption_translation == "emissions"] <- 
  "emission_add"

fixPropsBothSamples$consumption_translation[fixPropsBothSamples$consumption_translation == "environmental_friendliness"] <- 
  "rating_add"

# drop unused levels
fixPropsBothSamples$consumption_translation <- droplevels(fixPropsBothSamples$consumption_translation)

# remove unnecessary variables
rm(fixPropsOriginal_subset,
   fixPropsReplication_subset,
   common_conditions,
   common_cols)

###### Calculate fixed effects ------

fixed_effects_proportional_dwelltimes_combined <- function(dat){
  
  fixed_effects_price <- afex::mixed(price_all ~ (1 | task) + (1 | id) + consumption_translation * session * sample,
                                     data = dat,
                                     control = lmerControl(optimizer="bobyqa",
                                                           optCtrl = list(maxfun=2e5)),
                                     method = 'LRT')
  
  fixed_effects_consumption <- afex::mixed(consumption_all ~ (1 | task) + (1 | id) + consumption_translation * session * sample,
                                           data = dat,
                                           control = lmerControl(optimizer="bobyqa",
                                                                 optCtrl = list(maxfun=2e5)),
                                           method = 'LRT')
  
  fixed_effects_popularity <- afex::mixed(popularity_all ~ (1 | task) + (1 | id) + consumption_translation * session * sample,
                                          data = dat,
                                          control = lmerControl(optimizer="bobyqa",
                                                                optCtrl = list(maxfun=2e5)),
                                          method = 'LRT')
  
  
  return(list(fixed_effects_price = fixed_effects_price,
              fixed_effects_consumption = fixed_effects_consumption,
              fixed_effects_popularity = fixed_effects_popularity))
  
}

fixedEffectsProportionalDTCombined <- fixed_effects_proportional_dwelltimes_combined(fixPropsBothSamples)

###### Estimate marginal means ------

emm_proportional_dt_combined <- function(fixed_effects_list){
  
  get_contrasts <- function(model){
    
    emm <- emmeans(model, ~ consumption_translation * session * sample)
    
    # 1. Session effect within each condition and sample
    emm_session <- pairs(emm, reverse = TRUE, simple = "session")
    
    # 2. Compare each condition's session effect against control within each sample
    emm_vs_control <- contrast(emm,
                               interaction = list(
                                 consumption_translation = "trt.vs.ctrl",
                                 session = "consec"
                               ),
                               by = "sample")
    
    # 3. Compare session effects between conditions (all pairwise) within each sample
    emm_between_conditions <- contrast(emm,
                                       interaction = list(
                                         consumption_translation = "pairwise",
                                         session = "consec"
                                       ),
                                       by = "sample")
    
    # 4. Compare condition vs control contrasts between samples
    emm_vs_control_between_samples <- pairs(emm_vs_control,
                                            simple = "sample",
                                            reverse = TRUE)
    
    # 5. Compare session effects between samples within each condition
    emm_between_samples <- pairs(emm_session,
                                 simple = "sample",
                                 reverse = TRUE)
    
    return(list(
      emm = emm,
      emm_session = emm_session,
      emm_vs_control = emm_vs_control,
      emm_between_conditions = emm_between_conditions,
      emm_vs_control_between_samples = emm_vs_control_between_samples,
      emm_between_samples = emm_between_samples,
      confint_session = confint(emm_session),
      confint_vs_control = confint(emm_vs_control),
      confint_between_conditions = confint(emm_between_conditions),
      confint_vs_control_between_samples = confint(emm_vs_control_between_samples),
      confint_between_samples = confint(emm_between_samples)
    ))
  }
  
  return(list(
    price = get_contrasts(fixed_effects_list$fixed_effects_price),
    consumption = get_contrasts(fixed_effects_list$fixed_effects_consumption),
    popularity = get_contrasts(fixed_effects_list$fixed_effects_popularity)
  ))
}

emmProportionalDTCombined <- emm_proportional_dt_combined(fixedEffectsProportionalDTCombined)
