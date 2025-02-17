#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# last update: "2025-01-15"
# produced under R version: 2024.09.0
#---

# Reproduce behavioral results by adapting the script from Mertens et al. (2020): https://osf.io/jdep3 

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

df <- readRDS("data/df.rds")


# Product Choice -----

### Descriptives  ---------

# aggregate data on subject level
choice_prob_id <- df %>%
  group_by(session, condition, id) %>%
  summarise(mean_choice_id = mean(choice, na.rm = TRUE), 
            se_choice_id = sd(choice, na.rm = TRUE)/sqrt(sum(!is.na(choice)))
            ) %>%
  arrange(condition)

# aggregate data on group level
choice_prob_group <- choice_prob_id %>%
  group_by(session, condition) %>%
  summarise(mean_choice = mean(mean_choice_id, na.rm = TRUE), 
            se_choice = sd(mean_choice_id, na.rm = TRUE)/sqrt(sum(!is.na(mean_choice_id)))
            ) %>%
  arrange(condition)


### Choice Consistency in Session 1 between Conditions ------

# Subset the data for session 1
df_session1 <- subset(df, session == 1)

# Fit a model without the session interaction
choice_model_session1 <- glmer(choice ~ condition + (1 | id) + (1 | task), 
                               data = df_session1, 
                               family = binomial(link = "logit"),
                               control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun=2e5)))

# Perform an ANOVA to see if condition has a significant effect
Anova(choice_model_session1)


### Check for Significant Fixed Effects -------
fixed_effects_choice <- afex::mixed(choice ~ (session | id) + (1 | task) + session * consumption_translation * price_translation, 
                    data = df, 
                    family = binomial(link = "logit"), 
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun=2e5)),
                    method = 'LRT')



### Set up model ----

# Effect of translation of energy and water consumption (no translation/control) on product choice in absence and presence of a price translation. 
contrasts(df$consumption_translation) <- 
  contr.treatment(levels(df$consumption_translation), base = 1)
choice_model <- glmer(choice ~ (session | id) + (1 | task) + session * consumption_translation * price_translation, 
                      data = df, 
                      family = binomial(link = "logit"), 
                      control = glmerControl(optimizer="bobyqa", 
                                             optCtrl = list(maxfun=2e5)))

summary(choice_model)


### EMM Comparison -------

EMM <- emmeans(choice_model, 
               ~ consumption_translation * session * price_translation, 
               type = "response")

emm_session <- pairs(EMM, simple = "session")

emm_session_confint <- confint(emm_session)



# Attention -----

### Descriptives  ---------

# aggregate data on subject level
att_prob_id <- df %>%
  group_by(session, condition, id) %>%
  summarise(mean_diff_dt_id = mean(diff_t_options, na.rm = TRUE), 
            se_diff_dt_id = sd(diff_t_options, na.rm = TRUE)/sqrt(sum(!is.na(diff_t_options)))
  ) %>%
  arrange(condition)

# aggregate data on group level
att_prob_group <- att_prob_id %>%
  group_by(session, condition) %>%
  summarise(mean_diff_dt = mean(mean_diff_dt_id, na.rm = TRUE), 
            se_diff_dt = sd(mean_diff_dt_id, na.rm = TRUE)/sqrt(sum(!is.na(mean_diff_dt_id)))
  ) %>%
  arrange(condition)

### Attention Consistency in Session 1 between Conditions ------

# Fit a model without the session interaction
att_model_session1 <- lmer(diff_t_options ~ condition + (1 | id) + (1 | task), 
                               data = df_session1, 
                               REML = FALSE,
                               control = lmerControl(optimizer="bobyqa", 
                                                     optCtrl = list(maxfun=2e5)))

# Perform an ANOVA to see if condition has a significant effect
Anova(att_model_session1)


### Check for Significant Fixed Effects -------
fixed_effects_att <- afex::mixed(diff_t_options ~ (1 | id) + (1 | task) + session * consumption_translation * price_translation, 
                             data = df, 
                             REML = FALSE, 
                             control = lmerControl(optimizer="bobyqa", 
                                                    optCtrl = list(maxfun=2e5)),
                             method = 'LRT')


### Set up model -------

contrasts(df$consumption_translation) <- 
  contr.treatment(levels(df$consumption_translation), base = 1)
attention_model <- lmer(diff_t_options ~ (1 | id) + (1| task) + session * consumption_translation * price_translation, 
                        data = df, 
                        REML = FALSE, 
                        control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun=2e5)))

summary(attention_model)


### EMM Comparison -------

EMM_att <- emmeans(attention_model, 
               ~ consumption_translation * session * price_translation, 
               type = "response")

emm_session_att <- pairs(EMM_att, simple = "session")

emm_session_att_confint <- confint(emm_session_att)


# extract estimates
results_att <- as.data.frame(summary(emm_session_att_confint))[c('consumption_translation',
                                                                  'price_translation',
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

# change contrasts from session 1 - session 2 to session 2 - session 1
results_att[,c('estimate', 
               'asymp.LCL',
               'asymp.UCL')] <- results_att[,c('estimate', 
                                                     'asymp.LCL',
                                                     'asymp.UCL')] * - 1

# flip labels for upper and lower CI
results_att <- results_att %>%
  rename(
    lower_ci = asymp.UCL,
    upper_ci = asymp.LCL
  )



# Response Times ----

### Descriptives  ---------

# aggregate data on subject level
rt_prob_id <- df %>%
  group_by(session, condition, id) %>%
  summarise(mean_rt_id = mean(t_decision/1000, na.rm = TRUE), 
            se_rt_id = sd(t_decision/1000, na.rm = TRUE)/sqrt(sum(!is.na(t_decision/1000)))
  ) %>%
  arrange(condition)

# aggregate data on group level
rt_prob_group <- rt_prob_id %>%
  group_by(session, condition) %>%
  summarise(mean_rt = mean(mean_rt_id, na.rm = TRUE), 
            se_rt = sd(mean_rt_id, na.rm = TRUE)/sqrt(sum(!is.na(mean_rt_id)))
  ) %>%
  arrange(condition)


### RT Consistency in Session 1 between Conditions ------

# Fit a model without the session interaction
rt_model_session1 <- glmer(t_decision/1000 ~ condition + (1 | id) + (1 | task), 
                               data = df_session1, 
                               family = "inverse.gaussian"(link='identity'),
                               control = glmerControl(optimizer="bobyqa", 
                                                      optCtrl = list(maxfun=2e5)))

# Perform an ANOVA to see if condition has a significant effect
Anova(rt_model_session1)


### Check for Significant Fixed Effects -------
fixed_effects_rt <- afex::mixed(t_decision/1000 ~ (session | id) + (1 | task) + session * consumption_translation * price_translation, 
                                    data = df, 
                                    family = "Gamma"(link='identity'), 
                                    control = glmerControl(optimizer="bobyqa", 
                                                           optCtrl = list(maxfun=2e5)),
                                    method = 'LRT')


### Set up model ----

# Effect of translation of energy and water consumption (no translation/control) on product choice in absence and presence of a price translation. 
contrasts(df$consumption_translation) <- 
  contr.treatment(levels(df$consumption_translation), base = 1)
rt_model <- glmer(t_decision/1000 ~ (session | id) + (1 | task) + session * consumption_translation * price_translation, 
                      data = df, 
                  family = "Gamma"(link='identity'), 
                      control = glmerControl(optimizer="bobyqa", 
                                             optCtrl = list(maxfun=2e5)))

summary(rt_model)


### EMM Comparison -------

EMM_rt <- emmeans(rt_model, 
               ~ consumption_translation * session * price_translation, 
               type = "response")

emm_session_rt <- pairs(EMM_rt, simple = "session")

emm_session_rt_confint <- confint(emm_session_rt)


# extract estimates
results_rt <- as.data.frame(summary(emm_session_rt_confint))[c('consumption_translation',
                                                                 'price_translation',
                                                                 'estimate', 
                                                                 'SE',
                                                                 'asymp.LCL',
                                                                 'asymp.UCL')]

# round to four decimals
results_rt[,c('estimate', 
               'SE',
               'asymp.LCL',
               'asymp.UCL')] <- round(results_rt[,c('estimate', 
                                                     'SE',
                                                     'asymp.LCL',
                                                     'asymp.UCL')], 4)

# change contrasts from session 1 - session 2 to session 2 - session 1
results_rt[,c('estimate', 
               'asymp.LCL',
               'asymp.UCL')] <- results_rt[,c('estimate', 
                                               'asymp.LCL',
                                               'asymp.UCL')] * - 1

# flip labels for upper and lower CI
results_rt <- results_rt %>%
  rename(
    lower_ci = asymp.UCL,
    upper_ci = asymp.LCL
  )

# Save results ------

saveRDS(results_att, file = "data/results_att.rds")
saveRDS(results_rt, file = "data/results_rt.rds")
