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

# Descriptives of Product Choice ---------
desc_choice <- df %>%
  group_by(session, condition) %>%
  summarise(mean = mean(choice, na.rm = TRUE), sd = sd(choice, na.rm = TRUE)) %>%
  arrange(condition)

# Effects on Product Choice -----

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



# Effects on Attention -----

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



