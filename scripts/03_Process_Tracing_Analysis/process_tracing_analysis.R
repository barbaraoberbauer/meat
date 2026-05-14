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

### Inspect differences between conditions and sessions -------

calculate_session_differences <- function(fixPropsDat){
  
  aggFixPropsDat <- fixPropsDat %>%
    group_by(id, session, consumption_translation) %>%
    summarize(meanFixPropPrice = mean(price_all),
              meanFixPropConsumption = mean(consumption_all),
              meanFixPropPopularity = mean(popularity_all))
  
  aggFixPropsDat <- aggFixPropsDat %>%
    pivot_wider(id_cols = c(id, consumption_translation),
                names_from = session,
                values_from = c(contains("meanFix")),
                names_glue = "{.value}_s{session}") %>%
    filter(!if_any(everything(), is.na)) # filter subjects with missing values
  
  # calculate difference in proportional dwell times between sessions
  aggFixPropsDat$difFixPropPrice <- aggFixPropsDat$meanFixPropPrice_s2 -
    aggFixPropsDat$meanFixPropPrice_s1
  
  aggFixPropsDat$difFixPropConsumption <- aggFixPropsDat$meanFixPropConsumption_s2 -
    aggFixPropsDat$meanFixPropConsumption_s1
  
  aggFixPropsDat$difFixPropPopularity <- aggFixPropsDat$meanFixPropPopularity_s2 -
    aggFixPropsDat$meanFixPropPopularity_s1
  
  # keep only session values and transform to long format
  aggFixPropsDat_long <- aggFixPropsDat %>%
    select(id, consumption_translation, difFixPropPrice, difFixPropConsumption, difFixPropPopularity) %>%
    pivot_longer(cols = c(contains("difFix")),
                 names_to = c("attribute"),
                 values_to = "fixProp")
  
  return(aggFixPropsDat_long)
  
}

aggFixPropsDifOriginal <- calculate_session_differences(fixPropsOriginal)
aggFixPropsDifReplication <- calculate_session_differences(fixPropsReplication)

### Run one-sample t-tests vs. mu = 0 for each condition and attribute ---

# normality is not met for all attributes
aggFixPropsDifOriginal %>%
  group_by(consumption_translation, attribute) %>%
  summarise(p_shapiro = shapiro.test(fixProp)$p.value, .groups = "drop")

aggFixPropsDifReplication %>%
  group_by(consumption_translation, attribute) %>%
  summarise(p_shapiro = shapiro.test(fixProp)$p.value, .groups = "drop")

testAgainst0 <- function(aggFixProps){
  
  results <- aggFixProps %>%
    group_by(consumption_translation, attribute) %>%
    summarise(
      n        = n(),
      median   = median(fixProp, na.rm = TRUE),
      sd       = sd(fixProp, na.rm = TRUE),
      t_stat   = wilcox.test(fixProp, mu = 0)$statistic,
      df_t     = wilcox.test(fixProp, mu = 0)$parameter,
      p_value  = wilcox.test(fixProp, mu = 0)$p.value,
      ci_low   = wilcox.test(fixProp, mu = 0)$conf.int[1],
      ci_high  = wilcox.test(fixProp, mu = 0)$conf.int[2],
      .groups  = "drop"
    )
  
  # Apply FDR correction across all tests
  results <- results %>%
    mutate(p_adjusted = p.adjust(p_value, method = "BH"))
  
  return(results)
  
}

testAgainst0Original <- testAgainst0(aggFixPropsDifOriginal)
testAgainst0Replication <- testAgainst0(aggFixPropsDifReplication)


### Compare results to control groups ------

# Run Dunn's test per attribute, control as reference

testAgainstControl <- function(aggFixProps){
  
  results_within <- aggFixProps %>%
    group_by(sample, attribute) %>%
    summarise(
      dunn = list(
        dunnTest(fixProp ~ consumption_translation,
                 data = cur_data(),
                 method = "bh")$res %>%
          filter(grepl("control", Comparison))  # keep only comparisons involving control
      ),
      .groups = "drop"
    ) %>%
    unnest(dunn)
  
  return(results_within)

}

testAgainstCpntrolOriginal <- testAgainstControl(aggFixPropsDifOriginal)
testAgainstControlReplication <- testAgainstControl(aggFixPropsDifReplication)

### Combine datasets and check for differences between samples -----

aggFixPropsDifOriginal$sample <- "original"
aggFixPropsDifReplication$sample <- "replication"

aggFixPropsDifReplication$id <- as.character(aggFixPropsDifReplication$id)

aggFixPropsDifAll <- rbind(aggFixPropsDifOriginal, aggFixPropsDifReplication)

# filter comparable conditions

comparable_conditions <- c("control", "emissions", "environmental_friendliness",
                           "emission_add", "rating_add")

aggFixPropsDifAll <- aggFixPropsDifAll %>%
  filter(consumption_translation %in% comparable_conditions)

# rename conditions

aggFixPropsDifAll$consumption_translation[aggFixPropsDifAll$consumption_translation == "emissions"] <- "emission_add"

aggFixPropsDifAll$consumption_translation[aggFixPropsDifAll$consumption_translation == "environmental_friendliness"] <- "rating_add"

### Inspect differences between samples -------

# Run independent t-tests for each condition × attribute

# check for normality

# normality is not met for all attributes
aggFixPropsDifAll %>%
  group_by(consumption_translation, attribute, sample) %>%
  summarise(p_shapiro = shapiro.test(fixProp)$p.value, .groups = "drop")

results_between <- aggFixPropsDifAll %>%
  group_by(consumption_translation, attribute) %>%
  summarise(
    n_per_sample = list(table(sample)),
    mean_s1      = mean(fixProp[sample == "original"], na.rm = TRUE),
    mean_s2      = mean(fixProp[sample == "replication"], na.rm = TRUE),
    t_stat       = wilcox.test(fixProp ~ sample)$statistic,
    df_t         = wilcox.test(fixProp ~ sample)$parameter,
    p_value      = wilcox.test(fixProp ~ sample)$p.value,
    ci_low       = wilcox.test(fixProp ~ sample)$conf.int[1],
    ci_high      = wilcox.test(fixProp ~ sample)$conf.int[2],
    .groups      = "drop"
  ) %>%
  mutate(p_adjusted = p.adjust(p_value, method = "BH"))







