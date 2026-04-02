#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# purpose: inspect final fixations
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

# Calculate final fixation probability ----

# get final fixations
finalFixations <- dfReplicationProcess %>%
  group_by(id, session, task) %>%
  filter(fixNum == max(fixNum)) %>%
  ungroup()

# create variable that codes whether last fixated option was chosen
finalFixations <- finalFixations %>%
  mutate(lastFixatedChosen = case_when(
    grepl("NonEco", name) & choice == 0 ~ 1,
    !grepl("NonEco", name) & grepl("Eco", name) & choice == 1 ~ 1,
    TRUE ~ 0
  ))

# calculate probability to choose option that was last fixated
finalFixationsChoice <- finalFixations %>%
  group_by(id, session, consumption_translation) %>%
  summarize(pChooseLastFixated = mean(lastFixatedChosen))

# Plot final fixations ------

plotFinalFixations <- 
ggplot(data = finalFixationsChoice,
       aes(x = consumption_translation,
           y = pChooseLastFixated,
           fill = session)) +
  geom_hline(yintercept = 0.5,
             linetype = "dashed",
             linewidth = 1) +
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
       y = "Probability of Choosing \nLast Fixated Option",
       title = "Replication",
       fill = "Session") 


# Save plot
ggsave("figures/figureFinalFixations.pdf", 
       plotFinalFixations, 
       width = 12,
       height = 9,
       units = "in",
       device = cairo_pdf)
