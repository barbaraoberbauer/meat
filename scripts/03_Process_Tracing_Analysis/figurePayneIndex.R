#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# purpose: plot payne index
#---

# Load packages and read data ------

### Clear environment -------

#clear working environment
rm(list=ls())

#clear all plots
if(!is.null(dev.list())) dev.off()

### Install packages -------

# List of packages to check and install if necessary
packages <- c("tidyverse",
              "dplyr",
              "ggplot2",
              "patchwork")

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
library(dplyr)
library(ggplot2)
library(patchwork)

# Load theme
source("R/theme.R")
theme_set(themeMEAT())

rm(package, packages, is_package_installed)


### Load data ---------

load("data/preprocessedDataReplication.RData")

# Calculate within alternative and within attribute transitions ------

# dfReplicationProcess$withinAttribute <- NA
# dfReplicationProcess$withinAlternative <- NA
# 
# # within option
# 
# for (row in 1:nrow(dfReplicationProcess)) {
#   
#   if (dfReplicationProcess$fixNum[row] != 1) {
#     
#     # is the fixated option non-eco?
#     currentOption <- grepl("NonEco", dfReplicationProcess$name[row], fixed = TRUE)
#     previousOption <- grepl("NonEco", dfReplicationProcess$name[row-1], fixed = TRUE)
#     
#     # if both are either true or false, the transition was withinOption
#     ifelse(currentOption == previousOption, dfReplicationProcess$withinAlternative[row] <- 1,
#            dfReplicationProcess$withinAlternative[row] <- 0)
#     
#   }
#   
# }
# 
# # within attribute
# 
# for (row in 1:nrow(dfReplicationProcess)) {
#   
#   if (dfReplicationProcess$fixNum[row] != 1) {
#     
#     # is the fixated option non-eco?
#     currentAttribute <- str_remove(dfReplicationProcess$name[row], "NonEco")
#     currentAttribute <- str_remove(currentAttribute, "Eco")
#     
#     previousAttribute <- str_remove(dfReplicationProcess$name[row-1], "NonEco")
#     previousAttribute <- str_remove(previousAttribute, "Eco")
#     
#     # if both are either true or false, the transition was withinOption
#     ifelse(currentAttribute == previousAttribute, dfReplicationProcess$withinAttribute[row] <- 1,
#            dfReplicationProcess$withinAttribute[row] <- 0)
#     
#   }
#   
# }

# AI suggestion is faster 

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

# Calculate Payne Index -----

dfReplicationProcessSubset <- dfReplicationProcess %>%
  filter(fixNum != 1)

PayneIndex <- dfReplicationProcessSubset %>%
  group_by(id, session, consumption_translation) %>%
  summarize(PI = (sum(withinAlternative) - sum(withinAttribute))/
              (sum(withinAlternative) + sum(withinAttribute))
            )

# Plot Payne Index -----

plotPayneIndex <- 
ggplot(data = PayneIndex,
       aes(x = consumption_translation,
           y = PI,
           fill = session)) +
  geom_point(aes(color = session),
             position = position_jitterdodge(dodge.width = 0.5, 
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
  scale_fill_manual(values = scales::alpha(color_sessions, 0.4)) +  
  scale_color_manual(values = color_sessions) +
  scale_x_discrete(labels = labelsReplication) +
  labs(x = "Experimental Condition", 
       y = "Payne Index",
       title = "Replication",
       fill = "Session")
  
# Save plot
ggsave("figures/figurePayneIndex.pdf", 
       plotPayneIndex, 
       width = 9,
       height = 6,
       units = "in",
       device = cairo_pdf)


