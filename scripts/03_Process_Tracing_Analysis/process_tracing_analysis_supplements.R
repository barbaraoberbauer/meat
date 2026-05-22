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
              "FSA",
              "effectsize",
              "rjags"
              
)

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
library(effectsize)
library(rjags)


rm(package, packages, is_package_installed)


### Load data ---------

load("data/preprocessedDataOriginal.RData")
load("data/preprocessedDataReplication.RData")

runJagsOutReplication <- readRDS("data/modeling/runJagsOutmaaDDMDirichlet_replication_rating_add_20260518_2319.rds")

# Set sample -------

df_subset <- dfReplication %>%
  filter(consumption_translation == "rating_add")

# assign new ids that are starting from 1 and increment by 1
df_subset <- df_subset %>%
  mutate(id_new = dense_rank(id))

# sort data frame according to id_new (starting from 1 to last participant)
df_subset <- df_subset[order(df_subset$id_new),]

# sample size
SampleSize <- length(unique(df_subset$id_new))


# Derive attribute weights -----

# store as mcmc object
mcmcfin = as.mcmc.list(runJagsOutReplication$mcmc)

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



### SM Index at baseline ------

baseline_sm <- function(data){
  
  # Subset the data for session 1
  data_session1 <- subset(data, session == 1)
  
  # Fit a model without the session interaction
  sm_model_session1 <- lmer(SM ~ (1 | id) + (1 | task) + consumption_translation, 
                                 data = data_session1, 
                                 control = lmerControl(optimizer="bobyqa", 
                                                        optCtrl = list(maxfun=2e5))
  )
  
  return(sm_model_session1)
  
}

baselineSM <- baseline_sm(SMIndex)
Anova(baselineSM)

# check whether baseline sm index is significantly different from 0

# do some checks up front to see if t-test is valid

# histogram
SMIndexAg %>%
  filter(session == 1) %>%
  ggplot(aes(x = meanSM)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal()

# Q-Q plot
SMIndexAg %>%
  filter(session == 1) %>%
  ggplot(aes(sample = meanSM)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal()

# Shapiro-Wilk 
shapiro.test(SMIndexAg$meanSM[SMIndexAg$session == 1])

# Kolmogorov-Smirnov
ks.test(SMIndexAg$meanSM[SMIndexAg$session == 1], "pnorm",
        mean = mean(SMIndexAg$meanSM[SMIndexAg$session == 1]),
        sd = sd(SMIndexAg$meanSM[SMIndexAg$session == 1]))


# run t-test

ttestResults <- SMIndexAg %>%
  filter(session == 1) %>%
  group_by(consumption_translation) %>%
  summarize(
    t_test = list(t.test(meanSM, mu = 0)),
    cohens_d = list(cohens_d(meanSM, mu = 0))
  ) %>%
  mutate(
    estimate = sapply(t_test, function(x) x$estimate),
    df = sapply(t_test, function(x) x$parameter),
    t_value = sapply(t_test, function(x) x$statistic),
    p_value = sapply(t_test, function(x) x$p.value),
    conf_low = sapply(t_test, function(x) x$conf.int[1]),
    conf_high = sapply(t_test, function(x) x$conf.int[2]),
    d = sapply(cohens_d, function(x) x$Cohens_d),
    d_conf_low = sapply(cohens_d, function(x) x$CI_low),
    d_conf_high = sapply(cohens_d, function(x) x$CI_high)
  ) %>%
  select(-t_test, -cohens_d)


### Check for Significant Fixed Effects -------

fixed_effects_function <- function(data){
  
  fixed_effects <- afex::mixed(SM ~ (1 | id) + (1 | task) + session *
                                        consumption_translation, 
                                      data = data, 
                                      control = lmerControl(optimizer="bobyqa", 
                                                             optCtrl = list(maxfun=2e5)),
                                      method = 'LRT')
  
  return(fixed_effects)
  
}

fixedEffectsSMIndex <- fixed_effects_function(SMIndex)

### Run model ----

SMIndex_model <- function(data){
  
  contrasts(data$consumption_translation) <- 
    contr.treatment(levels(data$consumption_translation), base = 1)
  
  model <- lmer(SM ~ (1 | id) + (1 | task) + session *
                  consumption_translation, 
                 data = data, 
                 control = lmerControl(optimizer="bobyqa", 
                                        optCtrl = list(maxfun=2e5)))
  
  return(model)
  
  
}

SMIndexReplication <- SMIndex_model(SMIndex)

summary(SMIndexReplication)

### Calculate marginal means ----

emm_function <- function(sm_model){
  
  EMM <- emmeans(sm_model, 
                 ~ consumption_translation * session, 
                 type = "response")
  
  # compare between sessions
  emm_session <- pairs(EMM, reverse = TRUE, simple = "session")
  
  emm_session_confint <- confint(emm_session)
  
  # compare session effects to control group
  emm_vs_control <- contrast(EMM,
                             interaction = list(
                               session = "consec",
                               consumption_translation = "trt.vs.ctrl"
                             ),
                             type = "response")
  emm_vs_control_confint <- confint(emm_vs_control)
  
  return(list(
    emm_session = emm_session,
    emm_session_confint = emm_session_confint,
    emm_vs_control = emm_vs_control,
    emm_vs_control_confint = emm_vs_control_confint
  ))
  
}

emmReplication <- emm_function(fixedEffectsSMIndex)









