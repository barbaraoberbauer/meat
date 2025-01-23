#---
# title: "Mertens 2020 Reanalysis Revised - Behavioral Analysis" 
# date: "2023-04-17"
# last update: "2024-04-03"
# author: Barbara Oberbauer
#---

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
              "RColorBrewer")

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
library(RColorBrewer)


rm(package, packages, is_package_installed)


### Load data ---------

df <- readRDS("df.rds")
dfAllTrials <- readRDS("dfAllTrials.rds")

# Examination of Data --------

# Number of trials Environmental Friendliness Rating (Price-Translation absent vs. present)
sum(df$condition==7)
sum(df$condition==8)

# Sample Size
sampleSize <- length(unique(df$id))


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RQ1: Time point of first fixation of attribute translation at t1 vs. t2 ----------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# can't be answered with aggregated data

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RQ2: Changes of acquisition duration (dwell time) between t1 vs. t2 for each attribute ----------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Descriptives of fixations and dwell time ------------

# As a first step, aggregate the data for subjects and sessions
df_aggregated <- df %>%
  group_by(id, session) %>%
  summarise(# fixations
            f_price0 = mean(f_price0, na.rm = T),
            f_price1 = mean(f_price1, na.rm = T),
            f_consumption0 = mean(f_consumption0, na.rm = T),
            f_consumption1 = mean(f_consumption1, na.rm = T),
            f_popularity0 = mean(f_popularity0, na.rm = T),
            f_popularity1 = mean(f_popularity1, na.rm = T),
            f_consumption_translation0 = mean(f_consumption_translation0, na.rm = T),
            f_consumption_translation1 = mean(f_consumption_translation1, na.rm = T),
            f_price_translation0 = mean(f_price_translation0, na.rm = T),
            f_price_translation1 = mean(f_price_translation1, na.rm = T),
            f_choice0 = mean(f_choice0, na.rm = T),
            f_choice1 = mean(f_choice1, na.rm = T),
            f_total = mean(f_total, na.rm = T),
            # dwell time
            t_price0 = mean(t_price0, na.rm = T),
            t_price1 = mean(t_price1, na.rm = T),
            t_consumption0 = mean(t_consumption0, na.rm = T),
            t_consumption1 = mean(t_consumption1, na.rm = T),
            t_popularity0 = mean(t_popularity0, na.rm = T),
            t_popularity1 = mean(t_popularity1, na.rm = T),
            t_consumption_translation0 = mean(t_consumption_translation0, na.rm = T),
            t_consumption_translation1 = mean(t_consumption_translation1, na.rm = T),
            t_price_translation0 = mean(t_price_translation0, na.rm = T),
            t_price_translation1 = mean(t_price_translation1, na.rm = T),
            t_choice0 = mean(t_choice0, na.rm = T),
            t_choice1 = mean(t_choice1, na.rm = T),
            t_total = mean(t_total, na.rm = T),
            t_decision = mean(t_decision, na.rm = T),
            # differential dwell time
            diff_t_options = mean(diff_t_options, na.rm = T))

# Round data to two digits
df_aggregated[,3:ncol(df_aggregated)] <- round(df_aggregated[,3:ncol(df_aggregated)], digits = 2)

# Calculate descriptives
describe(df_aggregated$f_total) 
describe(df_aggregated$t_total) 
describe(df_aggregated$t_decision)

# Differential dwell time: (t_option1 - t_option0)/(t_option1 + t_option0)
describe(df$diff_t_options)


### Linear Mixed-Effects Models for all attributes --------

# price 0
price0 <- lmer(t_price0 ~ session * condition + (session | id) + (1| task),
                data = df, 
                REML = FALSE, 
                control = lmerControl(optimizer="bobyqa")
                )

summary(price0)


# price 1
price1 <- lmer(t_price1 ~ session * condition + (session | id) + (1| task), 
               data = df, 
               REML = FALSE, 
               control = lmerControl(optimizer="bobyqa")
               )

summary(price1)


# consumption 0
consumption0 <- lmer(t_consumption0 ~ session * condition + (session | id) + (1| task), 
                     data = df, 
                     REML = FALSE, 
                     control = lmerControl(optimizer="bobyqa")
                     )

summary(consumption0)

# consumption 1
consumption1 <- lmer(t_consumption1 ~ session * condition + (session | id) + (1| task), 
                     data = df, 
                     REML = FALSE, 
                     control = lmerControl(optimizer="bobyqa")
                     )

summary(consumption1)


# popularity 0
popularity0 <- lmer(t_popularity0 ~ session * condition + (session | id) + (1| task), 
                    data = df, 
                    REML = FALSE, 
                    control = lmerControl(optimizer="bobyqa")
                    )

summary(popularity0)


# popularity 1
popularity1 <- lmer(t_popularity1 ~ session * condition + (session | id) + (1| task), 
                    data = df, 
                    REML = FALSE, 
                    control = lmerControl(optimizer="bobyqa")
                    )

summary(popularity1)


### Aggregated Dwell Times for Condition 7 --------

df_cond7 <- df %>%
  filter(condition == 7)

# aggregate for each subject
df_cond7_subject <- df_cond7 %>%
  group_by(id, session) %>%
  summarise(# fixations
    f_price0 = mean(f_price0, na.rm = T),
    f_price1 = mean(f_price1, na.rm = T),
    f_consumption0 = mean(f_consumption0, na.rm = T),
    f_consumption1 = mean(f_consumption1, na.rm = T),
    f_popularity0 = mean(f_popularity0, na.rm = T),
    f_popularity1 = mean(f_popularity1, na.rm = T),
    f_consumption_translation0 = mean(f_consumption_translation0, na.rm = T),
    f_consumption_translation1 = mean(f_consumption_translation1, na.rm = T),
    f_choice0 = mean(f_choice0, na.rm = T),
    f_choice1 = mean(f_choice1, na.rm = T),
    f_total = mean(f_total, na.rm = T),
    # dwell time
    t_price0 = mean(t_price0, na.rm = T),
    t_price1 = mean(t_price1, na.rm = T),
    t_consumption0 = mean(t_consumption0, na.rm = T),
    t_consumption1 = mean(t_consumption1, na.rm = T),
    t_popularity0 = mean(t_popularity0, na.rm = T),
    t_popularity1 = mean(t_popularity1, na.rm = T),
    t_consumption_translation0 = mean(t_consumption_translation0, na.rm = T),
    t_consumption_translation1 = mean(t_consumption_translation1, na.rm = T),
    t_choice0 = mean(t_choice0, na.rm = T),
    t_choice1 = mean(t_choice1, na.rm = T),
    t_total = mean(t_total, na.rm = T),
    t_decision = mean(t_decision, na.rm = T),
    # differential dwell time
    diff_t_options = mean(diff_t_options, na.rm = T))

# aggregate for each session
df_cond7_group <- df_cond7_subject %>%
  group_by(session) %>%
  summarise(# fixations
    f_price0 = mean(f_price0, na.rm = T),
    f_price1 = mean(f_price1, na.rm = T),
    f_consumption0 = mean(f_consumption0, na.rm = T),
    f_consumption1 = mean(f_consumption1, na.rm = T),
    f_popularity0 = mean(f_popularity0, na.rm = T),
    f_popularity1 = mean(f_popularity1, na.rm = T),
    f_consumption_translation0 = mean(f_consumption_translation0, na.rm = T),
    f_consumption_translation1 = mean(f_consumption_translation1, na.rm = T),
    f_choice0 = mean(f_choice0, na.rm = T),
    f_choice1 = mean(f_choice1, na.rm = T),
    f_total = mean(f_total, na.rm = T),
    # dwell time
    t_price0 = mean(t_price0, na.rm = T),
    t_price1 = mean(t_price1, na.rm = T),
    t_consumption0 = mean(t_consumption0, na.rm = T),
    t_consumption1 = mean(t_consumption1, na.rm = T),
    t_popularity0 = mean(t_popularity0, na.rm = T),
    t_popularity1 = mean(t_popularity1, na.rm = T),
    t_consumption_translation0 = mean(t_consumption_translation0, na.rm = T),
    t_consumption_translation1 = mean(t_consumption_translation1, na.rm = T),
    t_choice0 = mean(t_choice0, na.rm = T),
    t_choice1 = mean(t_choice1, na.rm = T),
    t_total = mean(t_total, na.rm = T),
    t_decision = mean(t_decision, na.rm = T),
    # differential dwell time
    diff_t_options = mean(diff_t_options, na.rm = T))

# transpose data frame
#df_cond7_group <- t(df_cond7_group)


### Plot Mean Acquisition Time for Condition 7 -----------

colors <- c('honeydew', 'darkseagreen4')

# relevant variables: dwell times
dwell_time_cond7 <- df_cond7_group %>%
  select(session,
         t_price0,
         t_price1,
         t_consumption0,
         t_consumption1,
         t_popularity0,
         t_popularity1,
         t_consumption_translation0,
         t_consumption_translation1)

# transform to long format for plotting
dwell_time_cond7 <- gather(dwell_time_cond7,
                           attribute, dwell_time,
                           t_price0:t_consumption_translation1,
                           factor_key = TRUE)

# confidence intervals

# function to calculate confidence intervals
ci <- function(variable) {
  t.test(variable, conf.level = 0.95)$conf.int
}

# calcuate ci for each attribute and each session
ci_price0_sess1 <- ci(df_cond7_subject$t_price0[df_cond7_subject$session==1])
ci_price0_sess2 <- ci(df_cond7_subject$t_price0[df_cond7_subject$session==2])
ci_price1_sess1 <- ci(df_cond7_subject$t_price1[df_cond7_subject$session==1])
ci_price1_sess2 <- ci(df_cond7_subject$t_price1[df_cond7_subject$session==2])

ci_consumption0_sess1 <- ci(df_cond7_subject$t_consumption0[df_cond7_subject$session==1])
ci_consumption0_sess2 <- ci(df_cond7_subject$t_consumption0[df_cond7_subject$session==2])
ci_consumption1_sess1 <- ci(df_cond7_subject$t_consumption1[df_cond7_subject$session==1])
ci_consumption1_sess2 <- ci(df_cond7_subject$t_consumption1[df_cond7_subject$session==2])

ci_popularity0_sess1 <- ci(df_cond7_subject$t_popularity0[df_cond7_subject$session==1])
ci_popularity0_sess2 <- ci(df_cond7_subject$t_popularity0[df_cond7_subject$session==2])
ci_popularity1_sess1 <- ci(df_cond7_subject$t_popularity1[df_cond7_subject$session==1])
ci_popularity1_sess2 <- ci(df_cond7_subject$t_popularity1[df_cond7_subject$session==2])

ci_consumption_translation0_sess1 <- rep(NaN,2)
ci_consumption_translation0_sess2 <- ci(df_cond7_subject$t_consumption_translation0[df_cond7_subject$session==2])
ci_consumption_translation1_sess1 <- rep(NaN,2)
ci_consumption_translation1_sess2 <- ci(df_cond7_subject$t_consumption_translation1[df_cond7_subject$session==2])

# bind all cis 
cis <- rbind(ci_price0_sess1,
             ci_price0_sess2,
             ci_price1_sess1,
             ci_price1_sess2,
             ci_consumption0_sess1,
             ci_consumption0_sess2,
             ci_consumption1_sess1,
             ci_consumption1_sess2,
             ci_popularity0_sess1,
             ci_popularity0_sess2,
             ci_popularity1_sess1,
             ci_popularity1_sess2,
             ci_consumption_translation0_sess1,
             ci_consumption_translation0_sess2,
             ci_consumption_translation1_sess1,
             ci_consumption_translation1_sess2)

# transform to data frame
cis <- data.frame(cis)
colnames(cis) <- c('ci_min', 'ci_max')

# add cis to grouped data
dwell_time_cond7 <- cbind(dwell_time_cond7, cis)

# remove row names
rownames(dwell_time_cond7) <- NULL

ggplot(dwell_time_cond7, aes(x = attribute, y = dwell_time, fill = session)) +
  geom_bar(color = "black", position = position_dodge2(preserve = "single", padding = 0.2), stat = "identity", width = 0.5) + 
  geom_errorbar(aes(ymin = ci_min, ymax = ci_max), position = position_dodge(0.5), width=.2) + 
  labs(x = 'Attribute', y = 'Dwell Time', title = 'Condition 7 - Price Translation Absent', fill='Session') +
  theme(plot.caption = element_text(hjust=0.5, size=rel(1.5)),
        axis.title.x = element_text(hjust = 0.5,size = 12),
        axis.title.y = element_text(vjust = 0.5, size = 12),
        axis.text.x = element_text(size = 9, angle = -45),
        plot.margin = margin(t = 10,
                             r = 30,
                             b = 10,
                             l = 30),
        plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_discrete(labels=c("Price0", "Price1", "Consumption0", "Consumption1", "Popularity0", "Popularity1", "Translation0", "Translation1")) +
  scale_fill_manual(labels = c("Baseline", "Manipulation"), values = colors)
  
      
### Plot (Stacked) Mean Acquisition Time for Condition 7  -----------



# Remove variables that are no longer needed

rm(ci_price0_sess1,
   ci_price0_sess2,
   ci_price1_sess1,
   ci_price1_sess2,
   ci_consumption0_sess1,
   ci_consumption0_sess2,
   ci_consumption1_sess1,
   ci_consumption1_sess2,
   ci_popularity0_sess1,
   ci_popularity0_sess2,
   ci_popularity1_sess1,
   ci_popularity1_sess2,
   ci_consumption_translation0_sess1,
   ci_consumption_translation0_sess2,
   ci_consumption_translation1_sess1,
   ci_consumption_translation1_sess2)


### Aggregated Dwell Times for Condition 8 --------

df_cond8 <- df %>%
  filter(condition == 8)

# aggregate for each subject
df_cond8_subject <- df_cond8 %>%
  group_by(id, session) %>%
  summarise(# fixations
    f_price0 = mean(f_price0, na.rm = T),
    f_price1 = mean(f_price1, na.rm = T),
    f_consumption0 = mean(f_consumption0, na.rm = T),
    f_consumption1 = mean(f_consumption1, na.rm = T),
    f_popularity0 = mean(f_popularity0, na.rm = T),
    f_popularity1 = mean(f_popularity1, na.rm = T),
    f_consumption_translation0 = mean(f_consumption_translation0, na.rm = T),
    f_consumption_translation1 = mean(f_consumption_translation1, na.rm = T),
    f_choice0 = mean(f_choice0, na.rm = T),
    f_choice1 = mean(f_choice1, na.rm = T),
    f_total = mean(f_total, na.rm = T),
    # dwell time
    t_price0 = mean(t_price0, na.rm = T),
    t_price1 = mean(t_price1, na.rm = T),
    t_consumption0 = mean(t_consumption0, na.rm = T),
    t_consumption1 = mean(t_consumption1, na.rm = T),
    t_popularity0 = mean(t_popularity0, na.rm = T),
    t_popularity1 = mean(t_popularity1, na.rm = T),
    t_consumption_translation0 = mean(t_consumption_translation0, na.rm = T),
    t_consumption_translation1 = mean(t_consumption_translation1, na.rm = T),
    t_price_translation0 = mean(t_price_translation0, na.rm = T),
    t_price_translation1 = mean(t_price_translation1, na.rm = T),
    t_choice0 = mean(t_choice0, na.rm = T),
    t_choice1 = mean(t_choice1, na.rm = T),
    t_total = mean(t_total, na.rm = T),
    t_decision = mean(t_decision, na.rm = T),
    # differential dwell time
    diff_t_options = mean(diff_t_options, na.rm = T))

# aggregate for each session
df_cond8_group <- df_cond8_subject %>%
  group_by(session) %>%
  summarise(# fixations
    f_price0 = mean(f_price0, na.rm = T),
    f_price1 = mean(f_price1, na.rm = T),
    f_consumption0 = mean(f_consumption0, na.rm = T),
    f_consumption1 = mean(f_consumption1, na.rm = T),
    f_popularity0 = mean(f_popularity0, na.rm = T),
    f_popularity1 = mean(f_popularity1, na.rm = T),
    f_consumption_translation0 = mean(f_consumption_translation0, na.rm = T),
    f_consumption_translation1 = mean(f_consumption_translation1, na.rm = T),
    f_choice0 = mean(f_choice0, na.rm = T),
    f_choice1 = mean(f_choice1, na.rm = T),
    f_total = mean(f_total, na.rm = T),
    # dwell time
    t_price0 = mean(t_price0, na.rm = T),
    t_price1 = mean(t_price1, na.rm = T),
    t_consumption0 = mean(t_consumption0, na.rm = T),
    t_consumption1 = mean(t_consumption1, na.rm = T),
    t_popularity0 = mean(t_popularity0, na.rm = T),
    t_popularity1 = mean(t_popularity1, na.rm = T),
    t_consumption_translation0 = mean(t_consumption_translation0, na.rm = T),
    t_consumption_translation1 = mean(t_consumption_translation1, na.rm = T),
    t_price_translation0 = mean(t_price_translation0, na.rm = T),
    t_price_translation1 = mean(t_price_translation1, na.rm = T),
    t_choice0 = mean(t_choice0, na.rm = T),
    t_choice1 = mean(t_choice1, na.rm = T),
    t_total = mean(t_total, na.rm = T),
    t_decision = mean(t_decision, na.rm = T),
    # differential dwell time
    diff_t_options = mean(diff_t_options, na.rm = T))


### Plot Mean Acquisition Time for Condition 8 -----------

# relevant variables: dwell times
dwell_time_cond8 <- df_cond8_group %>%
  select(session,
         t_price0,
         t_price1,
         t_consumption0,
         t_consumption1,
         t_popularity0,
         t_popularity1,
         t_consumption_translation0,
         t_consumption_translation1,
         t_price_translation0,
         t_price_translation1)

# transform to long format for plotting
dwell_time_cond8 <- gather(dwell_time_cond8,
                           attribute, dwell_time,
                           t_price0:t_price_translation1,
                           factor_key = TRUE)

# confidence intervals

# calcuate ci for each attribute and each session
ci_price0_sess1 <- ci(df_cond8_subject$t_price0[df_cond8_subject$session==1])
ci_price0_sess2 <- ci(df_cond8_subject$t_price0[df_cond8_subject$session==2])
ci_price1_sess1 <- ci(df_cond8_subject$t_price1[df_cond8_subject$session==1])
ci_price1_sess2 <- ci(df_cond8_subject$t_price1[df_cond8_subject$session==2])

ci_consumption0_sess1 <- ci(df_cond8_subject$t_consumption0[df_cond8_subject$session==1])
ci_consumption0_sess2 <- ci(df_cond8_subject$t_consumption0[df_cond8_subject$session==2])
ci_consumption1_sess1 <- ci(df_cond8_subject$t_consumption1[df_cond8_subject$session==1])
ci_consumption1_sess2 <- ci(df_cond8_subject$t_consumption1[df_cond8_subject$session==2])

ci_popularity0_sess1 <- ci(df_cond8_subject$t_popularity0[df_cond8_subject$session==1])
ci_popularity0_sess2 <- ci(df_cond8_subject$t_popularity0[df_cond8_subject$session==2])
ci_popularity1_sess1 <- ci(df_cond8_subject$t_popularity1[df_cond8_subject$session==1])
ci_popularity1_sess2 <- ci(df_cond8_subject$t_popularity1[df_cond8_subject$session==2])

ci_consumption_translation0_sess1 <- rep(NaN,2)
ci_consumption_translation0_sess2 <- ci(df_cond8_subject$t_consumption_translation0[df_cond8_subject$session==2])
ci_consumption_translation1_sess1 <- rep(NaN,2)
ci_consumption_translation1_sess2 <- ci(df_cond8_subject$t_consumption_translation1[df_cond8_subject$session==2])

ci_price_translation0_sess1 <- rep(NaN,2)
ci_price_translation0_sess2 <- ci(df_cond8_subject$t_price_translation0[df_cond8_subject$session==2])
ci_price_translation1_sess1 <- rep(NaN,2)
ci_price_translation1_sess2 <- ci(df_cond8_subject$t_price_translation1[df_cond8_subject$session==2])


# bind all cis 
cis <- rbind(ci_price0_sess1,
             ci_price0_sess2,
             ci_price1_sess1,
             ci_price1_sess2,
             ci_consumption0_sess1,
             ci_consumption0_sess2,
             ci_consumption1_sess1,
             ci_consumption1_sess2,
             ci_popularity0_sess1,
             ci_popularity0_sess2,
             ci_popularity1_sess1,
             ci_popularity1_sess2,
             ci_consumption_translation0_sess1,
             ci_consumption_translation0_sess2,
             ci_consumption_translation1_sess1,
             ci_consumption_translation1_sess2,
             ci_price_translation0_sess1,
             ci_price_translation0_sess2,
             ci_price_translation1_sess1,
             ci_price_translation1_sess2)

# transform to data frame
cis <- data.frame(cis)
colnames(cis) <- c('ci_min', 'ci_max')

# add cis to grouped data
dwell_time_cond8 <- cbind(dwell_time_cond8, cis)

# remove row names
rownames(dwell_time_cond8) <- NULL

ggplot(dwell_time_cond8, aes(x = attribute, y = dwell_time, fill = session)) +
  geom_bar(color = "black", position = position_dodge2(preserve = "single", padding = 0.2), stat = "identity", width = 0.5) + 
  geom_errorbar(aes(ymin = ci_min, ymax = ci_max), position = position_dodge(0.5), width=.2) + 
  labs(x = 'Attribute', y = 'Dwell Time', title = 'Condition 8 - Price Translation Present', fill='Session') +
  theme(plot.caption = element_text(hjust=0.5, size=rel(1.5)),
        axis.title.x = element_text(hjust = 0.5,size = 12),
        axis.title.y = element_text(vjust = 0.5, size = 12),
        axis.text.x = element_text(size = 9, angle = -45),
        plot.margin = margin(t = 10,
                             r = 30,
                             b = 10,
                             l = 30),
        plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_discrete(labels=c("Price0", "Price1", "Consumption0", "Consumption1", "Popularity0", "Popularity1", "Consumption Trans0", "Consumption Trans1", "Price Trans0", "Price Trans1")) +
  scale_fill_manual(labels = c("Baseline", "Manipulation"), values = colors)


### Plot (Stacked) Mean Acquisition Time for Condition 8  -----------


# remove variables
rm(ci_price0_sess1,
   ci_price0_sess2,
   ci_price1_sess1,
   ci_price1_sess2,
   ci_consumption0_sess1,
   ci_consumption0_sess2,
   ci_consumption1_sess1,
   ci_consumption1_sess2,
   ci_popularity0_sess1,
   ci_popularity0_sess2,
   ci_popularity1_sess1,
   ci_popularity1_sess2,
   ci_consumption_translation0_sess1,
   ci_consumption_translation0_sess2,
   ci_consumption_translation1_sess1,
   ci_consumption_translation1_sess2,
   ci_price_translation0_sess1,
   ci_price_translation0_sess2,
   ci_price_translation1_sess1,
   ci_price_translation1_sess2)



### Aggregated Dwell Times for Condition 1 --------

df_cond1 <- df %>%
  filter(condition == 1)

# aggregate for each subject
df_cond1_subject <- df_cond1 %>%
  group_by(id, session) %>%
  summarise(# fixations
    f_price0 = mean(f_price0, na.rm = T),
    f_price1 = mean(f_price1, na.rm = T),
    f_consumption0 = mean(f_consumption0, na.rm = T),
    f_consumption1 = mean(f_consumption1, na.rm = T),
    f_popularity0 = mean(f_popularity0, na.rm = T),
    f_popularity1 = mean(f_popularity1, na.rm = T),
    f_choice0 = mean(f_choice0, na.rm = T),
    f_choice1 = mean(f_choice1, na.rm = T),
    f_total = mean(f_total, na.rm = T),
    # dwell time
    t_price0 = mean(t_price0, na.rm = T),
    t_price1 = mean(t_price1, na.rm = T),
    t_consumption0 = mean(t_consumption0, na.rm = T),
    t_consumption1 = mean(t_consumption1, na.rm = T),
    t_popularity0 = mean(t_popularity0, na.rm = T),
    t_popularity1 = mean(t_popularity1, na.rm = T),
    t_choice0 = mean(t_choice0, na.rm = T),
    t_choice1 = mean(t_choice1, na.rm = T),
    t_total = mean(t_total, na.rm = T),
    t_decision = mean(t_decision, na.rm = T),
    # differential dwell time
    diff_t_options = mean(diff_t_options, na.rm = T))

# aggregate for each session
df_cond1_group <- df_cond1_subject %>%
  group_by(session) %>%
  summarise(# fixations
    f_price0 = mean(f_price0, na.rm = T),
    f_price1 = mean(f_price1, na.rm = T),
    f_consumption0 = mean(f_consumption0, na.rm = T),
    f_consumption1 = mean(f_consumption1, na.rm = T),
    f_popularity0 = mean(f_popularity0, na.rm = T),
    f_popularity1 = mean(f_popularity1, na.rm = T),
    f_choice0 = mean(f_choice0, na.rm = T),
    f_choice1 = mean(f_choice1, na.rm = T),
    f_total = mean(f_total, na.rm = T),
    # dwell time
    t_price0 = mean(t_price0, na.rm = T),
    t_price1 = mean(t_price1, na.rm = T),
    t_consumption0 = mean(t_consumption0, na.rm = T),
    t_consumption1 = mean(t_consumption1, na.rm = T),
    t_popularity0 = mean(t_popularity0, na.rm = T),
    t_popularity1 = mean(t_popularity1, na.rm = T),
    t_choice0 = mean(t_choice0, na.rm = T),
    t_choice1 = mean(t_choice1, na.rm = T),
    t_total = mean(t_total, na.rm = T),
    t_decision = mean(t_decision, na.rm = T),
    # differential dwell time
    diff_t_options = mean(diff_t_options, na.rm = T))


### Plot Mean Acquisition Time for Control Condition -----------

# relevant variables: dwell times
dwell_time_cond1 <- df_cond1_group %>%
  select(session,
         t_price0,
         t_price1,
         t_consumption0,
         t_consumption1,
         t_popularity0,
         t_popularity1)

# transform to long format for plotting
dwell_time_cond1 <- gather(dwell_time_cond1,
                           attribute, dwell_time,
                           t_price0:t_popularity1,
                           factor_key = TRUE)

# confidence intervals

# calcuate ci for each attribute and each session
ci_price0_sess1 <- ci(df_cond1_subject$t_price0[df_cond1_subject$session==1])
ci_price0_sess2 <- ci(df_cond1_subject$t_price0[df_cond1_subject$session==2])
ci_price1_sess1 <- ci(df_cond1_subject$t_price1[df_cond1_subject$session==1])
ci_price1_sess2 <- ci(df_cond1_subject$t_price1[df_cond1_subject$session==2])

ci_consumption0_sess1 <- ci(df_cond1_subject$t_consumption0[df_cond1_subject$session==1])
ci_consumption0_sess2 <- ci(df_cond1_subject$t_consumption0[df_cond1_subject$session==2])
ci_consumption1_sess1 <- ci(df_cond1_subject$t_consumption1[df_cond1_subject$session==1])
ci_consumption1_sess2 <- ci(df_cond1_subject$t_consumption1[df_cond1_subject$session==2])

ci_popularity0_sess1 <- ci(df_cond1_subject$t_popularity0[df_cond1_subject$session==1])
ci_popularity0_sess2 <- ci(df_cond1_subject$t_popularity0[df_cond1_subject$session==2])
ci_popularity1_sess1 <- ci(df_cond1_subject$t_popularity1[df_cond1_subject$session==1])
ci_popularity1_sess2 <- ci(df_cond1_subject$t_popularity1[df_cond1_subject$session==2])


# bind all cis 
cis <- rbind(ci_price0_sess1,
             ci_price0_sess2,
             ci_price1_sess1,
             ci_price1_sess2,
             ci_consumption0_sess1,
             ci_consumption0_sess2,
             ci_consumption1_sess1,
             ci_consumption1_sess2,
             ci_popularity0_sess1,
             ci_popularity0_sess2,
             ci_popularity1_sess1,
             ci_popularity1_sess2)

# transform to data frame
cis <- data.frame(cis)
colnames(cis) <- c('ci_min', 'ci_max')

# add cis to grouped data
dwell_time_cond1 <- cbind(dwell_time_cond1, cis)

# remove row names
rownames(dwell_time_cond1) <- NULL

ggplot(dwell_time_cond1, aes(x = attribute, y = dwell_time, fill = session)) +
  geom_bar(color = "black", position = position_dodge2(preserve = "single", padding = 0.2), stat = "identity", width = 0.5) + 
  geom_errorbar(aes(ymin = ci_min, ymax = ci_max), position = position_dodge(0.5), width=.2) + 
  labs(x = 'Attribute', y = 'Dwell Time', title = 'Condition 1 - Control Condition', fill='Session') +
  theme(plot.caption = element_text(hjust=0.5, size=rel(1.5)),
        axis.title.x = element_text(hjust = 0.5,size = 12),
        axis.title.y = element_text(vjust = 0.5, size = 12),
        axis.text.x = element_text(size = 9, angle = -45),
        plot.margin = margin(t = 10,
                             r = 30,
                             b = 10,
                             l = 30),
        plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_discrete(labels=c("Price0", "Price1", "Consumption0", "Consumption1", "Popularity0", "Popularity1")) +
  scale_fill_manual(labels = c("Baseline", "Manipulation"), values = colors)

# remove unnecessary variables
rm(ci_price0_sess1,
    ci_price0_sess2,
    ci_price1_sess1,
    ci_price1_sess2,
    ci_consumption0_sess1,
    ci_consumption0_sess2,
    ci_consumption1_sess1,
    ci_consumption1_sess2,
    ci_popularity0_sess1,
    ci_popularity0_sess2,
    ci_popularity1_sess1,
    ci_popularity1_sess2)

rm(cis)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RQ3: Probability of Choosing the Eco-Friendly Option depending on Fixations of Consumption AT -----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Condition 7 - Is Difference in Acquisition Frequency correlated with Eco-Choice Probability? ------

# Difference in acquisition frequency per choice option 
df <- mutate(df, diff_f_consumption_translations = ((f_consumption_translation1 - f_consumption_translation0)))


EcoChoiceDiffFixationsCond7 <- df %>%
  filter(session == 2 & condition == 7) %>%
  group_by(diff_f_consumption_translations) %>%
  summarize(PEcoChoice = round(mean(choice),2),
            N = n())

EcoChoiceDiffFixationsCond7 <- EcoChoiceDiffFixationsCond7 %>%
  rename(DiffFixations = diff_f_consumption_translations)


# plot relation between fixation differences and eco choices

ggplot(EcoChoiceDiffFixationsCond7, aes(x = DiffFixations, y = PEcoChoice)) +
  geom_point(size = 3) +  
  geom_smooth(se = FALSE, color = 'darkseagreen4') +
  labs(x = 'Fixation Difference Eco - NonEco', y = 'Proportion Eco Choices', title = 'Condition 7 - Price Translation Absent') +
  theme(plot.caption = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.title.x = element_text(hjust = 0.5,size = 12, margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(vjust = 0.5, size = 12, margin = margin(t = 0, r = 20, b = 0, l = 0)), # plots y axis label not so close to axis
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.margin = margin(t = 10,
                             r = 30,
                             b = 10,
                             l = 30),
        plot.title = element_text(hjust = 0.5, margin = margin(t = 0, r = 0, b = 20, l = 0))
  ) +
  scale_x_continuous(breaks = c(-4, -3, -2, -1, 0, 1, 2, 3))


### Condition 8 - Is Difference in Acquisition Frequency correlated with Eco-Choice Probability? ------

EcoChoiceDiffFixationsCond8 <- df %>%
  filter(session == 2 & condition == 8) %>%
  group_by(diff_f_consumption_translations) %>%
  summarize(PEcoChoice = round(mean(choice),2),
            N = n())

EcoChoiceDiffFixationsCond8 <- EcoChoiceDiffFixationsCond8 %>%
  rename(DiffFixations = diff_f_consumption_translations)


# plot relation between fixation differences and eco choices

ggplot(EcoChoiceDiffFixationsCond8, aes(x = DiffFixations, y = PEcoChoice)) +
  geom_point(size = 3) +  
  geom_smooth(se = FALSE, color = 'darkseagreen4') +
  labs(x = 'Fixation Difference Eco - NonEco', y = 'Proportion Eco Choices', title = 'Condition 8 - Price Translation Present') +
  theme(plot.caption = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.title.x = element_text(hjust = 0.5,size = 12, margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(vjust = 0.5, size = 12, margin = margin(t = 0, r = 20, b = 0, l = 0)), # plots y axis label not so close to axis
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.margin = margin(t = 10,
                             r = 30,
                             b = 10,
                             l = 30),
        plot.title = element_text(hjust = 0.5, margin = margin(t = 0, r = 0, b = 20, l = 0))
  ) +
  scale_x_continuous(breaks = c(-4, -3, -2, -1, 0, 1, 2, 3, 4))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RQ4: Relationship between Attribute Weight and First Fixation/Acquisition Frequency -------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Logistic Regressions

### Create data frame containing task values -----

# read csv file containing choice problems 
choiceProblems <- read.table("choiceProblems.txt", header = F)

# remove not-needed columns
choiceProblems <- choiceProblems[, -c(2,5,7,8,10,13,15,16)]

# rename columns
colnames(choiceProblems) <- c('task','Price_A', 'Energy_A', 'Water_A', 'Popularity_A', 'Price_B', 'Energy_B', 'Water_B', 'Popularity_B')

# convert to numeric + data frame
choiceProblems <- apply(choiceProblems, 2, function(x) gsub(",", "", x))
choiceProblems <- apply(choiceProblems, 2, as.numeric)
choiceProblems <- as.data.frame(choiceProblems)


# convert to euro values (see paper)
# PriceA
choiceProblems[choiceProblems[,2] == 669, 2] <- 529
choiceProblems[choiceProblems[,2] == 1059, 2] <- 939
choiceProblems[choiceProblems[,2] == 1449, 2] <- 1349

# PriceB
choiceProblems[choiceProblems[,6] == 669, 6] <- 529
choiceProblems[choiceProblems[,6] == 1059, 6] <- 939
choiceProblems[choiceProblems[,6] == 1449, 6] <- 1349


# add attribute translations to choice problems

choiceProblems$OperationCosts_A <- NA
choiceProblems$OperationCosts_B <- NA
choiceProblems$CarbonEmissions_A <- NA
choiceProblems$CarbonEmissions_B <- NA
choiceProblems$EnvironmentRating_A <- NA
choiceProblems$EnvironmentRating_B <- NA

# fill attribute translations

# Operating Costs A
choiceProblems$OperationCosts_A[choiceProblems[,3] == 160] <- 111
choiceProblems[choiceProblems[,3] == 160, 10] <- 1078.26
choiceProblems[choiceProblems[,3] == 190, 10] <- 1255.34
choiceProblems[choiceProblems[,3] == 220, 10] <- 1432.41

# Carbon Emissions A
choiceProblems[choiceProblems[,3] == 160, 12] <- 112
choiceProblems[choiceProblems[,3] == 190, 12] <- 134
choiceProblems[choiceProblems[,3] == 220, 12] <- 155

# Environmental Rating A
choiceProblems[choiceProblems[,3] == 160, 14] <- 5
choiceProblems[choiceProblems[,3] == 190, 14] <- 3
choiceProblems[choiceProblems[,3] == 220, 14] <- 1

# Operating Costs B
choiceProblems[choiceProblems[,7] == 160, 11] <- 1078.26
choiceProblems[choiceProblems[,7] == 190, 11] <- 1255.34
choiceProblems[choiceProblems[,7] == 220, 11] <- 1432.41

# Carbon Emissions B
choiceProblems[choiceProblems[,7] == 160, 13] <- 112
choiceProblems[choiceProblems[,7] == 190, 13] <- 134
choiceProblems[choiceProblems[,7] == 220, 13] <- 155

# Environmental Rating B
choiceProblems[choiceProblems[,7] == 160, 15] <- 5
choiceProblems[choiceProblems[,7] == 190, 15] <- 3
choiceProblems[choiceProblems[,7] == 220, 15] <- 1


### Compute differences in values between attributes ---------

# create additional columns for differences
additionalColumns <- c('dPrice', 'dEnergy', 'dWater', 'dPopularity', 'dOperationCosts', 'dCarbonEmissions', 'dEnvironmentalRating')
choiceProblems[, additionalColumns] <- NA

#is option A or option B ecological (=lower energy use)?
mask <- choiceProblems$Energy_A > choiceProblems$Energy_B

#fill columns with computed differences (EcoOption - NonEcoOption)
for (i in 1:nrow(choiceProblems)) { 
  
  if (mask[i] == T) {
    
    choiceProblems[i,16] <- choiceProblems[i, 6] - choiceProblems[i, 2] #price
    choiceProblems[i,17] <- choiceProblems[i, 7] - choiceProblems[i, 3] #energy
    choiceProblems[i,18] <- choiceProblems[i, 8] - choiceProblems[i, 4] #water
    choiceProblems[i,19] <- choiceProblems[i, 9] - choiceProblems[i, 5] #popularity
    choiceProblems[i,20] <- choiceProblems[i, 11] - choiceProblems[i, 10] #operation costs
    choiceProblems[i,21] <- choiceProblems[i, 13] - choiceProblems[i, 12] #carbon emissions
    choiceProblems[i,22] <- choiceProblems[i, 15] - choiceProblems[i, 14] #environmental rating
    
  } else {
    
    choiceProblems[i,16] <- choiceProblems[i, 2] - choiceProblems[i, 6] #price
    choiceProblems[i,17] <- choiceProblems[i, 3] - choiceProblems[i, 7] #energy
    choiceProblems[i,18] <- choiceProblems[i, 4] - choiceProblems[i, 8] #water
    choiceProblems[i,19] <- choiceProblems[i, 5] - choiceProblems[i, 9] #popularity
    choiceProblems[i,20] <- choiceProblems[i, 10] - choiceProblems[i, 11] #operation costs
    choiceProblems[i,21] <- choiceProblems[i, 12] - choiceProblems[i, 13] #carbon emissions
    choiceProblems[i,22] <- choiceProblems[i, 14] - choiceProblems[i, 15] #environmental rating
    
  }
  
}

 
### Standardize Differences -----

# Standardizing
scaled_columns <- apply(choiceProblems[, 16:ncol(choiceProblems)], 2, function (x) scale(x, center = T, scale = T))
scaled_columns <- as.data.frame(scaled_columns)
colnames(scaled_columns) <- c("sPrice", "sEnergy", "sWater", "sPopularity", "sOperatingCosts", "sCarbonEmissions", "sEnvironmentalRating")

# Add columns to choiceProblems data frame
choiceProblems <- cbind(choiceProblems, scaled_columns)
saveRDS(choiceProblems, file = "choiceProblems.rds")

# after scaling, energy and water consumption result in the same values --> continue using energy as proxy

# Create new data set with scaled variables 
scaled_columns <- choiceProblems[, c(1, 23:26, 29)]

# create new data frame 
df2 <- merge(df, scaled_columns, by = "task", all.x = T) 


### Logistic regression models - Attribute Weights -------

# create subsets for each session
subset_session1 <- df2[df2$session==1 & (df2$condition==7 | df2$condition==8), ]
subset_session2 <- df2[df2$session==2 & (df2$condition==7 | df2$condition==8), ]

# Logistic regression first session
Sess1_Cond78 <- glmer(choice ~ sPrice + sEnergy + sPopularity + (1 | task) + (1 | id), 
                      data = subset_session1, 
                      family = "binomial")

summary(Sess1_Cond78)

car::Anova(Sess1_Cond78, type=3)

#Logistic regression second session
Sess2_Cond78 <- glmer(choice ~ sPrice + sEnergy + sPopularity + (1 | task) + (1 | id), 
                      data = subset_session2, 
                      family = "binomial")

summary(Sess2_Cond78)

car::Anova(Sess2_Cond78, type=3)


### Single Subject Regression Fit ---------

data <- df2[df2$session==2 & (df2$condition==7 | df2$condition==8), ]

ids <- unique(data$id)
n_ids <- length(ids)

# set up data frame to store coefficients of glm (random effects not necessary since we are fitting per participant)
glm_coeff <- data.frame(matrix(nrow = n_ids, ncol = 3))
colnames(glm_coeff) <- c("Price", "Energy", "Popularity")

###### Fit Coefficients for each Subject -----

for (s in 1:n_ids){
  # select data
  data_id <- data[data$id==ids[s],]
  
  # specify choice and attribute values
  glmy <- data_id$choice
  glmx <- cbind(data_id$sPrice,data_id$sEnergy,data_id$sPopularity)
  
  # fit glm
  glmres <- glm.fit(glmx,glmy,family = binomial())
  
  # store coefficients
  glm_coeff[s,] <- glmres$coefficients
  
}

###### Remove Outlier Coefficients -----
incl_ids <- rep(1, n_ids)

for (a in 1:3){
  cutoffs <- mean(glm_coeff[,a],na.rm = T) + c(1,-1) * 4 * sd(glm_coeff[,a],na.rm = T)
  incl_ids = incl_ids*((glm_coeff[,a] < cutoffs[1]) & (glm_coeff[,a] > cutoffs[2]))
}

#glm_coeff <- glm_coeff[incl_ids != 0 | !is.na(incl_ids), ]
glm_coeff <- glm_coeff[incl_ids == 1,]

# exclude all rows that only contain NaNs
glm_coeff <- glm_coeff[rowSums(is.na(glm_coeff))==0,]


###### Plot Coefficients -----

col_coeff <- brewer.pal(4, "BuPu")

# Define the colors and labels of plot
legend_labels <- c("Price", "Energy", "Popularity")
#legend_colors <- c(col_coeff[2], col_coeff[3], col_coeff[4])

legend_colors <- c("Price" = col_coeff[2], "Energy" = col_coeff[3], "Popularity" = col_coeff[4])

# Plot
ggplot(glm_coeff, aes(x=c(1:nrow(glm_coeff)), y=Price)) +
  geom_point(aes(y=sort(Price), color = "Price"), size = 4) +
  geom_point(aes(y=sort(Energy), color = "Energy"), alpha = 0.9, size = 4) +
  geom_point(aes(y=sort(Popularity), color = "Popularity"), alpha = 0.4, size = 4) +
  geom_hline(yintercept=0, linewidth=1.1) + 
  labs(x = 'Index', y = 'Coefficients', title = 'Individual Subject Coefficients', color = "Legend") +
  scale_color_manual(values = legend_colors,
                     name = "Attribute") +
  theme(plot.caption = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.title.x = element_text(hjust = 0.5,size = 12, margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(vjust = 0.5, size = 12, margin = margin(t = 0, r = 20, b = 0, l = 0)), # plots y axis label not so close to axis
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.margin = margin(t = 10,
                             r = 30,
                             b = 10,
                             l = 30),
        plot.title = element_text(hjust = 0.5, margin = margin(t = 0, r = 0, b = 20, l = 0))
  )
               

###### T-Tests on Coefficients ------

t.test(glm_coeff$Price)
t.test(glm_coeff$Energy)
t.test(glm_coeff$Popularity)


# Over the entire data set (n=701) and across both sessions, all effects are significant
# In the subsets, only some of the effects reach significance
# Taking both sessions in the subsets, all effects reach significance

# Results form individual glm fits and model across subjects yield contradictory results

rm(scaled_columns,
   a,
   additionalColumns,
   col_coeff,
   i,
   glmy,
   incl_ids,
   legend_colors,
   legend_labels,
   mask,
   s)


#++++++++++++++++++++++
# Sanity Checks -------
#++++++++++++++++++++++

### Correlation of Attributes -----

# add attribute values to data frame that includes all trials
dfAllTrials <- merge(dfAllTrials, choiceProblems[,1:9], by = "task", all.x = T)

# extract attribute values from option A
Attribute_Values_A <- dfAllTrials[ , c("Price_A", "Energy_A", "Water_A", "Popularity_A")]
colnames(Attribute_Values_A) <- c("Price", "Energy", "Water", "Popularity")

# extract attribute values from option B
Attribute_Values_B <- dfAllTrials[ , c("Price_B", "Energy_B", "Water_B", "Popularity_B")]
colnames(Attribute_Values_B) <- c("Price", "Energy", "Water", "Popularity")

Attribute_Values <- rbind(Attribute_Values_A, Attribute_Values_B)

#correlations
cor.test(Attribute_Values$Price, Attribute_Values$Energy, method = "spearman", exact = F)
cor.test(Attribute_Values$Popularity, Attribute_Values$Energy, method = "spearman", exact = F)
cor.test(Attribute_Values$Price, Attribute_Values$Popularity, method = "spearman", exact = F)

# Attributes correlate significantly with each other (around ~ 0.5)


### Plot P(EcoChoice) for all Levels of all Attributes --------

# create data subset
session <- 2

# use df (filtered for non-conflict trials) instead of dfAllTrials (because it's unclear to us right now
# what choice codes when both alternatives have the same energy consumption)
df_attributes <- df[df$session == session & (df$condition == 7 | df$condition == 8), ]
df_attributes <- merge(df_attributes, choiceProblems[, 1:15], by = "task", all.x = T)

# add variable to indicate whether Option A or B was more energy efficient
df_attributes$mask <- df_attributes$Energy_A < df_attributes$Energy_B

# function to determine PChoice for levels of one attribute
attribute <- function(attribute_A, attribute_B){
  
  # Convert attribute_A and attribute_B to symbols
  attribute_A <- enquo(attribute_A)
  attribute_B <- enquo(attribute_B)
  
  # determine how often attribute of Option A was chosen
  Option_A <- df_attributes %>%
    group_by(!!attribute_A) %>%
    summarize(NChoice = sum(choice == mask))
  
  # determine how often attribute of Option B was chosen
  Option_B <- df_attributes %>%
    group_by(!!attribute_B) %>%
    summarize(NChoice = sum(choice != mask))
  
  # add, summarize and divide by total choices made in data frame Attribute
  Attribute <- Option_A
  colnames(Attribute) <- c('Levels', 'NChoice')
  
  Attribute$NChoice <- Attribute$NChoice + Option_B$NChoice
  
  Attribute$NChoice <- Attribute$NChoice / nrow(df_attributes)
  # change colnames to PChoice (choice probability) instead of NChoice (choice count)
  colnames(Attribute) <- c('Levels', 'PChoice')
  
  return(Attribute)
}


Price <- attribute(Price_A, Price_B)
Energy <- attribute(Energy_A, Energy_B)
Popularity <- attribute(Popularity_A, Popularity_B)
Rating <- attribute(EnvironmentRating_A, EnvironmentRating_B)

# add column with name of attribute to results
Price$attribute <- "price"
Energy$attribute <- "consumption"
Popularity$attribute <- "popularity"
Rating$attribute <- "rating"

# bind together to one data frame
PChoice_Attributes <- rbind(Price, Energy, Popularity, Rating)

# rename levels
levels <- rep(c('low', 'medium', 'high'), 4)
PChoice_Attributes$levels <- levels

PChoice_Attributes <- PChoice_Attributes[,2:4]

# plot

col_attributes <- brewer.pal(3, "Oranges")

PChoice_Attributes$levels <- factor(PChoice_Attributes$levels, levels = unique(PChoice_Attributes$levels))

ggplot(PChoice_Attributes, aes(x = attribute, y = PChoice, fill = levels)) +
  geom_bar(color = 'black', position = position_dodge2(preserve = "single", padding = 0.2), stat = "identity", width = 0.5) +
  labs(x = "Attributes", y = "PChoice", title = "Choice Probability per Attribute Level (Session 2)", fill = "Levels") +
  #specify colors
  scale_fill_manual(values = col_attributes) +
  scale_x_discrete(labels = c("consumption" = "Consumption", "popularity" = "Popularity", "price" = "Price", "rating" = "Rating")) +
  theme(plot.caption = element_text(hjust=0.5, size=rel(1.5), margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.title.x = element_text(hjust = 0.5,size = 12),
        axis.title.y = element_text(vjust = 0.5, size = 12, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.text.x = element_text(size = 9, angle = -45),
        plot.margin = margin(t = 10,
                             r = 30,
                             b = 10,
                             l = 30),
        plot.title = element_text(hjust = 0.5)
  ) 



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RQ5: Fixation Duration of Consumption relative to other attributes -------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# compute relative dwell time (consumption/all)
df3 <- dplyr::mutate(df, RelTimeCons = rowSums(df[,c("t_consumption0", 
                                                     "t_consumption1", 
                                                     "t_consumption_translation0", 
                                                     "t_consumption_translation1")], na.rm = T)/
                       rowSums(df[,c("t_option0","t_option1")], na.rm = T))

Rel_Dwell_Time <- df3 %>%
  group_by(id, session) %>%
  summarize(RelTimeCons = mean(RelTimeCons))

# exclude subjects with missing values for any session
excl_sub <- Rel_Dwell_Time$id[is.na(Rel_Dwell_Time$RelTimeCons)]
Rel_Dwell_Time <- Rel_Dwell_Time[!Rel_Dwell_Time$id %in% excl_sub, ]

# exclude participants who only have one session
check <- Rel_Dwell_Time %>% count(id)
excl_sub <- check$id[check$n == 1]
Rel_Dwell_Time <- Rel_Dwell_Time[!Rel_Dwell_Time$id %in% excl_sub, ]

t.test(Rel_Dwell_Time$RelTimeCons[Rel_Dwell_Time$session == 1], Rel_Dwell_Time$RelTimeCons[Rel_Dwell_Time$session == 2], paired = T)
mean(Rel_Dwell_Time$RelTimeCons[Rel_Dwell_Time$session == 1])
mean(Rel_Dwell_Time$RelTimeCons[Rel_Dwell_Time$session == 2])

# longer dwell time on consumption attributes in Session 2 compared to Session 1


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RQ6: Inspect Response Times ----------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# aggregate response times for id, session, and choice
response_times <- df %>%
  filter(condition == 7 | condition == 8) %>%
  group_by(id, session, choice) %>%
  summarize(RT = mean(t_decision)) 

# change data structure and RT to seconds
response_times$choice <- as.factor(response_times$choice)
response_times$RT <- response_times$RT/1000

# calculate means
mean(response_times$RT[response_times$session == 1 & response_times$choice == 0])
mean(response_times$RT[response_times$session == 1 & response_times$choice == 1])
mean(response_times$RT[response_times$session == 2 & response_times$choice == 0])
mean(response_times$RT[response_times$session == 2 & response_times$choice == 1])

# plot response time densities

colors_rt <- c("darkgrey", "#6A51A3")

response_times %>%
  ggplot(aes(RT, fill = choice)) +
  geom_density(bw = 1.5, color = "black", alpha = .5, position = "identity") +
  facet_grid(~session, drop=TRUE, scales = "free", space = "free",
             labeller = labeller(session = c("1" = "Baseline", "2" = "Manipulation"))) +
  labs(x = 'Response Time (s)' , y = 'Density', title = '') +
  scale_fill_manual(values = colors_rt, name = 'Response', labels = c('Non-Eco', 'Eco')) +
  theme_bw() +
  theme(axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        plot.margin = margin(t = 10,
                             r = 30,
                             b = 10,
                             l = 30),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 12)
  ) 

# plot RT quantile probabilities

# calculate quantiles
quantiles <- response_times %>%
  group_by(session, choice) %>%
  reframe(rt = quantile(RT, probs = seq(0.1, 0.9, 0.2)))

quantiles$quantiles <- rep(c(0.1, 0.3, 0.5, 0.7, 0.9), 4)

# calculate choice probability
choice_probability <- df %>%
  filter(condition %in% c(7, 8)) %>%
  group_by(id, session, choice) %>%
  summarize(count = n()) %>%
  group_by(id, session) %>%
  mutate(PChoice = count / sum(count))

choice_probability_aggregated <- choice_probability %>%
  group_by(session) %>%
  mutate(TotalPChoice = sum(PChoice)) %>%
  mutate(PChoice = PChoice / TotalPChoice) %>%
  select(-TotalPChoice) %>%
  group_by(session, choice) %>%
  summarize(PChoice = sum(PChoice))

# add choice probability to quantile data frame
choice_probability_aggregated$choice <- as.factor(choice_probability_aggregated$choice)

quantiles <- quantiles %>%
  left_join(choice_probability_aggregated, by = c("session", "choice"))

quantiles$cumulativePChoice <- quantiles$quantiles * quantiles$PChoice

# plot
ggplot(quantiles, aes(x = rt, y = cumulativePChoice, color = session, linetype = choice)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 4) +
  labs(x = 'Response Time (s)' , y = 'Cumulative Probability', title = '') +
  scale_color_manual(values = colors_rt, name = 'Session', labels = c('Baseline', 'Manipulation')) +
  scale_linetype_manual(name = 'Choice', labels = c('Non-Eco', 'Eco'), values = c('solid', 'dashed')) +
  theme_bw() +
  theme(axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        plot.margin = margin(t = 10,
                             r = 30,
                             b = 10,
                             l = 30),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 12)
  )

saveRDS(quantiles, file="cumulative_probabilities_data.rds")

ggsave("Cumulative_Probability_RT.png", width = 12, height = 10, units = "cm", dpi = 300)

