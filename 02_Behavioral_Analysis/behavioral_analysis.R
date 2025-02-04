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
              "lmerTest")

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


rm(package, packages, is_package_installed)


### Load data ---------

df <- readRDS("data/df.rds")

# Descriptives of Product Choice ---------
desc_choice <- df %>%
  group_by(session, condition) %>%
  summarise(mean = mean(choice, na.rm = TRUE), sd = sd(choice, na.rm = TRUE)) %>%
  arrange(condition)

# Effects on Product Choice -----

# Assess choice consistency in Session 1 between conditions
# Subset the data for session 1
df_session1 <- subset(df, session == 1)

# Fit a model without the session interaction
choice_model_session1 <- glmer(choice ~ condition + (1 | id) + (1 | task), 
                               data = df_session1, 
                               family = binomial(link = "logit"),
                               control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun=2e5)))

# Perform an ANOVA to see if condition has a significant effect
Anova(choice_model_session1)



# Consistency in product choice in control condition.
choice_base <- glmer(choice ~ (session | id) + (1 | task) + session * condition, 
                     data = df, 
                     family = binomial(link = "logit"), 
                     control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun=2e5)))
Anova(choice_base)
summary(choice_base)
exp(cbind(est = fixef(choice_base)[1:2],
          confint(choice_base,
                  parm = "beta_", 
                  level = 0.95))) # Odds ratios and confidence intervals.


### Control ----

# Effect of translation of energy and water consumption (no translation/control) on product choice in absence and presence of a price translation. 
contrasts(df$consumption_translation) <- 
  contr.treatment(levels(df$consumption_translation), base = 1)
choice_cntrl <- glmer(choice ~ (session | id) + (1 | task) + session * consumption_translation * price_translation, 
                      data = df, 
                      family = binomial(link = "logit"), 
                      control = glmerControl(optimizer="bobyqa", 
                                             optCtrl = list(maxfun=2e5)))
Anova(choice_cntrl)

### Operating Costs ------

# Effect of translation of energy and water consumption (operating costs) on product choice in absence and presence of a price translation.
contrasts(df$consumption_translation) <- 
  contr.treatment(levels(df$consumption_translation), base = 2)
choice_csts <- glmer(choice ~ (session | id) + (1 | task) + session * consumption_translation * price_translation, 
                     data = df, 
                     family = binomial(link = "logit"), 
                     control = glmerControl(optimizer="bobyqa", 
                                            optCtrl = list(maxfun=2e5)))
summary(choice_csts)
exp(cbind(est = fixef(choice_csts), confint(choice_csts, 
                                            parm = "beta_", 
                                            level = 0.95))) # Odds ratios and confidence intervals.


### Carbon Emissions -----

# Effect of translation of energy and water consumption (carbon emissions) on product choice in absence and presence of a price translation.
contrasts(df$consumption_translation) <- 
  contr.treatment(levels(df$consumption_translation), base = 3)
choice_crbn <- glmer(choice ~ (session | id) + (1 | task) + session * consumption_translation * price_translation, 
                     data = df, 
                     family = binomial(link = "logit"), 
                     control = glmerControl(optimizer="bobyqa", 
                                            optCtrl = list(maxfun=2e5)))
summary(choice_crbn)
exp(cbind(est = fixef(choice_crbn), confint(choice_crbn, 
                                            parm = "beta_", 
                                            level = 0.95))) # Odds ratios and confidence intervals.


### Rating -----

# Effect of translation of energy and water consumption (environmental friendliness rating) on product choice in absence and presence of a price translation. 
contrasts(df$consumption_translation) <- contr.treatment(levels(df$consumption_translation), base = 4)
choice_rtng <- glmer(choice ~ (session | id) + (1 | task) + session * consumption_translation * price_translation, 
                     data = df, 
                     family = binomial(link = "logit"), 
                     control = glmerControl(optimizer="bobyqa", 
                                            optCtrl = list(maxfun=2e5)))
summary(choice_rtng)
exp(cbind(est = fixef(choice_rtng), confint(choice_rtng, 
                                            parm = "beta_", 
                                            level = 0.95))) # Odds ratios and confidence intervals.


# Effects on Attention -----

# original random effects structure is not supported for our data (due to singularity)
attention_base <- lmer(diff_t_options ~ (session | id) + (1| task) + session * condition, 
                       data = df, 
                       REML = FALSE, 
                       control = lmerControl(optimizer="bobyqa", 
                                             optCtrl = list(maxfun=2e5)))

anova(attention_base)
summary(attention_base)

# reduced model
attention_red <- lmer(diff_t_options ~ session * condition + (1 | id) + (1| task), 
                       data = df, 
                       REML = FALSE, 
                       control = lmerControl(optimizer="bobyqa", 
                                             optCtrl = list(maxfun=2e5)))
Anova(attention_red)
summary(attention_red)

contr <- diag(length(fixef(attention_red)))[11:16,]
contest(attention_red, L = contr)


# Assess choice consistency in Session 1 between conditions

# Fit a model without the session interaction
attention_session1 <- lmer(diff_t_options ~ condition + (1 | id) + (1 | task), 
                               data = df_session1, 
                               REML = FALSE,
                               control = lmerControl(optimizer="bobyqa", 
                                                     optCtrl = list(maxfun=2e5)))

# Perform an ANOVA to see if condition has a significant effect
Anova(attention_session1)

### Control -------

contrasts(df$consumption_translation) <- 
  contr.treatment(levels(df$consumption_translation), base = 1)
attention_cntrl <- lmer(diff_t_options ~ (1 | id) + (1| task) + session * consumption_translation * price_translation, 
                        data = df, 
                        REML = FALSE, 
                        control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun=2e5)))
Anova(attention_cntrl)
summary(attention_cntrl)

### Operating Costs -------

contrasts(df$consumption_translation) <- 
  contr.treatment(levels(df$consumption_translation), base = 2)
attention_csts <- lmer(diff_t_options ~ (1 | id) + (1| task) + session * consumption_translation , 
                       data = df, 
                       REML = FALSE, 
                       control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun=2e5)))
summary(attention_csts)
confint(attention_csts, parm = "beta_", level = 0.95) # Confidence intervals.

### Carbon emissions ------

contrasts(df$consumption_translation) <- 
  contr.treatment(levels(df$consumption_translation), base = 3)
attention_crbn <- lmer(diff_t_options ~ (1 | id) + (1| task) + session * consumption_translation , 
                       data = df, 
                       REML = FALSE, 
                       control = lmerControl(optimizer="bobyqa", 
                                             optCtrl = list(maxfun=2e5)))
summary(attention_crbn)
confint(attention_crbn, parm = "beta_", level = 0.95) # Confidence intervals.

### Rating ------

contrasts(df$consumption_translation) <- 
  contr.treatment(levels(df$consumption_translation), base = 4)
attention_rtng <- lmer(diff_t_options ~ (1 | id) + (1| task) + session * consumption_translation * price_translation, 
                       data = df, 
                       REML = FALSE, 
                       control = lmerControl(optimizer="bobyqa", 
                                             optCtrl = list(maxfun=2e5)))
summary(attention_rtng)
confint(attention_rtng, parm = "beta_", level = 0.95) # Confidence intervals.
