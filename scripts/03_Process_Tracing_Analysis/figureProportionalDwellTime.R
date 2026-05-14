#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# purpose: plot dwell time proportions
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

load("data/preprocessedDataOriginal.RData")
load("data/preprocessedDataReplication.RData")


# Calculate dwell time proportions ------

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

# Inspect differences between conditions at baseline --------

fixPropsOriginal_long <- pivot_longer(fixPropsOriginal,
                     cols = c(contains("_"), -consumption_translation),
                     names_to = c("attribute", "option"),
                     names_sep = "_",
                     values_to = "fixProp")

fixPropsReplication_long <- pivot_longer(fixPropsReplication,
                                      cols = c(contains("_"), -consumption_translation),
                                      names_to = c("attribute", "option"),
                                      names_sep = "_",
                                      values_to = "fixProp")


# Plot Session 1

plotFixPropsBaseline <- function(dat, labels, title){
  
  plot <- dat %>%
    filter(session == 1 & option == "all") %>%
    ggplot(aes(x = fixProp,
               fill = attribute)) +
    geom_density(color = "black",
                 linewidth = 1) +
    facet_wrap(~consumption_translation, 
               #nrow = 4,
               ncol = 1,
               labeller = labeller(consumption_translation = labels)) +
    scale_fill_manual(values = scales::alpha(color_attributes, 0.5),
                      labels = c("price" = "Price",
                                 "consumption" = "Consumption",
                                 "popularity" = "Popularity"),
                      breaks = c("price",
                                 "consumption",
                                 "popularity")) + 
    labs(x = "Proportional Dwell Time (Session 1)", 
         y = "Density",
         title = title,
         fill = "Attribute") +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 12,
                                    face = "bold"),  # style for facet labels
          panel.border = element_rect(color = "black", fill = NA),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
    )
  
  return(plot)
  
}

plotFixPropsBaselineOriginal <- plotFixPropsBaseline(fixPropsOriginal_long, labelsOriginal, "Original")
plotFixPropsBaselineReplication <- plotFixPropsBaseline(fixPropsReplication_long, labelsReplication, "Replication")

# Combine plots

plotFixPropBaselineAll <- 
  plotFixPropsBaselineOriginal + 
  plotFixPropsBaselineReplication + 
  plot_layout(guides = 'collect',
              axis_title = 'collect') &
  theme(legend.position = 'bottom')

# Save plot
ggsave("figures/figureProporitonalDwellTimeBaseline.pdf", 
       plotFixPropBaselineAll, 
       width = 10,
       height = 10,
       units = "in",
       device = cairo_pdf)



# Inspect differences between conditions and sessions ----

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

# Plot differences

plotFixPropsDif <- function(dat, labels, title){
  
  dat %>%
    ggplot(aes(x = fixProp,
               fill = attribute)) +
    geom_density(color = "black",
                 linewidth = 1) +
    geom_vline(xintercept = 0,
               linetype = "dashed",
               linewidth = 1,
               color = "black") +
    facet_wrap(~consumption_translation, 
               nrow = 1,
               labeller = labeller(consumption_translation = labels)) + 
    scale_fill_manual(values = scales::alpha(color_attributes, 0.6),
                      labels = labelsDwellTimeProportions,
                      breaks = c("difFixPropPrice", 
                                 "difFixPropConsumption", 
                                 "difFixPropPopularity")) +
    labs(x = "Difference between Proportional Dwell Time (Session 2 - Session 1)", 
         y = "Density",
         title = title,
         fill = "Attribute") +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 12,
                                    face = "bold"),  # style for facet labels
          panel.border = element_rect(color = "black", fill = NA),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
    )
  
}

plotFixPropDifOriginal <- plotFixPropsDif(aggFixPropsDifOriginal, labelsOriginal, "Study 1")
plotFixPropDifReplication <- plotFixPropsDif(aggFixPropsDifReplication, labelsReplication, "Study 2")

# Combine plots

plotFixPropDifAll <- 
  plotFixPropDifOriginal /
  plot_spacer() /
  plotFixPropDifReplication + 
  plot_layout(heights = c(1, 0.3, 1),
              guides = 'collect',
              axis_title = 'collect') &
  theme(legend.position = 'bottom',
        plot.margin = margin(2, 2, 2, 2))

# Save plot
ggsave("figures/figureProporitonalDwellTimeDifferences.png", 
       plotFixPropDifAll, 
       width = 10,
       height = 7,
       units = "in")
 









