#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# last update: "2025-01-15"
# produced under R version: 2024.09.0
#---

# Process tracing analysis

# Load packages and read data ------------

#clear working environment
rm(list=ls())

#clear all plots
if(!is.null(dev.list())) dev.off()


### Install packages -------

# List of packages to check and install if necessary
packages <- c("tidyverse", "psych", "ggplot2")

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
library(ggplot2)

rm(package, packages, is_package_installed)


### Load data ---------

df <- readRDS("data/df.rds")

# add dwell time differences to data frame
df$ddt_eco_noneco <- (df$t_option1 - df$t_option0)/1000 # in sec

# add normalized dwell time differences to data frame
df$ddt_eco_noneco_norm <- (df$t_option1 - df$t_option0)/(df$t_option1 + df$t_option0)

# subset data
df_pt_absent <- df %>%
  filter(price_translation == 0)


# Normalized Probability Choosing Eco - Dwell Time Differences --------

# number of bins
n_bins <- 10

chooseEcoProbabilityNorm <- df %>%
  group_by(session, consumption_translation) %>%
  mutate(bin = ntile(ddt_eco_noneco_norm, n_bins)) %>%
  group_by(session, consumption_translation, bin) %>%
  summarize(binMean = mean(ddt_eco_noneco_norm),
            ecoProb = mean(choice == 1),
            seEcoProb = sqrt(mean(choice == 1) * (1 - mean(choice == 1)) / n()))

### Plot data ------

translation_labels <- c(
  "control" = "Control",
  "operating_costs" = "Operating Costs",
  "emissions" = "Carbon Emissions",
  "environmental_friendliness" = "Rating"
)

# session colors
cols_sess <- c("#225780", "#8CC5E3")

ggplot(chooseEcoProbabilityNorm, aes(x = binMean, y = ecoProb, color = session)) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.7) +
  geom_errorbar(aes(ymin = ecoProb - 2 * seEcoProb, 
                    ymax = ecoProb + 2 * seEcoProb),
                linewidth = 1) +
  scale_color_manual(values = cols_sess) +
  facet_wrap(~consumption_translation, 
             nrow = 2,
             labeller = labeller(consumption_translation = translation_labels)) +
  labs(x = "Normalized dwell time difference (eco - other)", 
       y = "P(Eco Choice)", 
       color = "Session") +
  theme_bw() +
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        plot.margin = margin(t = 15,
                             r = 15,
                             b = 15,
                             l = 15),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size = 13),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), size = 13),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.key.size = unit(0.5, "cm"),
        legend.position = "top",
        strip.background = element_rect(fill = "white", color = "black"),
        strip.text = element_text(size = 12)  # style for facet labels
  )



# Fixation proportions ---------

### Preparations -----

# fixProps -> acquistion time for each attribute proportional to the total duration of the trial (vector containing six elements in our case)
fixProps <- data.frame(price0 = rep(NA, nrow(df_pt_absent)),
                       consumption0 = rep(NA, nrow(df_pt_absent)),
                       translation0 = rep(NA, nrow(df_pt_absent)),
                       popularity0 = rep(NA, nrow(df_pt_absent)),
                       price1 = rep(NA, nrow(df_pt_absent)),
                       consumption1 = rep(NA, nrow(df_pt_absent)),
                       translation1 = rep(NA, nrow(df_pt_absent)),
                       popularity1 = rep(NA, nrow(df_pt_absent))) 

# attributes and their translation are treated as one attribute for simplicity
fixProps$price0 <- df_pt_absent$t_price0/1000 #rowSums(df_subset[, c("t_price0", "t_price_translation0")], na.rm = TRUE)/1000
fixProps$consumption0 <- df_pt_absent$t_consumption0/1000
fixProps$translation0 <- df_pt_absent$t_consumption_translation0/1000
fixProps$popularity0 <- df_pt_absent$t_popularity0/1000
fixProps$price1 <- df_pt_absent$t_price1/1000 #rowSums(df_subset[, c("t_price1", "t_price_translation1")], na.rm = TRUE)/1000
fixProps$consumption1 <- df_pt_absent$t_consumption1/1000
fixProps$translation1 <- df_pt_absent$t_consumption_translation1/1000
fixProps$popularity1 <- df_pt_absent$t_popularity1/1000

# divide by total duration of the trial
fixProps <- fixProps/abs(df_pt_absent$t_decision) #take absolute value instead of +/- coded RT

# normalize each trial to 1
fixProps <- fixProps/rowSums(fixProps, na.rm = TRUE)

# add info about id and session
fixProps$id <- df_pt_absent$id
fixProps$session <- df_pt_absent$session
fixProps$consumption_translation <- df_pt_absent$consumption_translation

# aggregate data over subjects
fixPropsAgg <- fixProps %>%
  group_by(id, session, consumption_translation) %>%
  summarize(price0 = mean(price0, na.rm = TRUE),
            consumption0 = mean(consumption0, na.rm = TRUE),
            translation0 = mean(translation0, na.rm = TRUE),
            popularity0 = mean(popularity0, na.rm = TRUE),
            price1 = mean(price1, na.rm = TRUE),
            consumption1 = mean(consumption1, na.rm = TRUE),
            translation1 = mean(translation1, na.rm = TRUE),
            popularity1 = mean(popularity1, na.rm = TRUE))

# turn into appropriate data frame
fixPropsAggWide <- fixPropsAgg %>%
  pivot_wider(
    id_cols = c(id, consumption_translation),
    names_from = session,
    values_from = c(price0, consumption0, translation0, popularity0, price1, consumption1, translation1, popularity1),
    names_glue = "{.value}_{session}"
  )


### Calculate session difference ----
fixPropsDif <- fixPropsAggWide %>%
  group_by(id, consumption_translation) %>%
  summarize(price0_diff = price0_2 - price0_1,
            consumption0_diff = consumption0_2 - consumption0_1,
            translation0_diff = translation0_2,
            popularity0_diff = popularity0_2 - popularity0_1,
            price1_diff = price1_2 - price1_1,
            consumption1_diff = consumption1_2 - consumption1_1,
            translation1_diff = translation1_2,
            popularity1_diff = popularity1_2 - popularity1_1)

### Subtract control group means -------

# separate control group data
fixPropsDifControl <- fixPropsDif %>%
  ungroup() %>%
  filter(consumption_translation == "control") %>%
  select(-c(id, consumption_translation))

# calculate mean for later subtraction
meanFixPropsDifControl <- colMeans(fixPropsDifControl, na.rm = TRUE)

# remove control group from fixPropsDif
fixPropsDif <- fixPropsDif %>%
  filter(consumption_translation != "control")

# subtract control group means
fixPropsDif$price0_diff <- fixPropsDif$price0_diff - meanFixPropsDifControl[1]
fixPropsDif$consumption0_diff <- fixPropsDif$consumption0_diff - meanFixPropsDifControl[2]
fixPropsDif$popularity0_diff <- fixPropsDif$popularity0_diff - meanFixPropsDifControl[4]

fixPropsDif$price1_diff <- fixPropsDif$price1_diff - meanFixPropsDifControl[5]
fixPropsDif$consumption1_diff <- fixPropsDif$consumption1_diff - meanFixPropsDifControl[6]
fixPropsDif$popularity1_diff <- fixPropsDif$popularity1_diff - meanFixPropsDifControl[8]

### Plot data ---

fixPropsDif_exclTranslation <- fixPropsDif %>%
  select(-c(translation0_diff, translation1_diff))

# bring into long format
fixPropsDif_exclTranslation <- fixPropsDif_exclTranslation %>%
  pivot_longer(
    cols = "price0_diff":"popularity1_diff",
    names_to = "attribute",
    values_to = "propDwellTimeDiff"
  )

fixPropsDif_exclTranslation$attribute <- factor(fixPropsDif_exclTranslation$attribute, levels = c("price0_diff",
                                                                                                  "price1_diff",
                                                                                                  "consumption0_diff",
                                                                                                  "consumption1_diff",
                                                                                                  "popularity0_diff",
                                                                                            "popularity1_diff"))

attribute_labels <- c(
  "price0_diff" = "Price \nOther",
  "price1_diff" = "Price \nEco",
  "consumption0_diff" = "Consum \nOther",
  "consumption1_diff" = "Consum \nEco",
  "popularity0_diff" = "Popularity \nOther",
  "popularity1_diff" = "Popularity \nEco")


ggplot(fixPropsDif_exclTranslation, aes(x = attribute, y = propDwellTimeDiff)) +
  geom_hline(yintercept = 0, color = "darkred", linewidth = 2, linetype = "dashed") +
  stat_summary(fun = "mean", 
               size = 1,
               position = position_dodge(width = 0.3)) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               position = position_dodge(width = 0.3),
               linewidth = 1.2,
               width = 0.15) +
  facet_grid(~consumption_translation, 
             labeller = labeller(consumption_translation = c("operating_costs" = "Operating Costs",
                                                             "emissions" = "Carbon Emissions",
                                                             "environmental_friendliness" = "Rating"))) +
  scale_x_discrete(labels = attribute_labels) +
  labs(x = "Attributes", 
       y = "Normalized Dwell Time Differences (Session 2 - Session 1)") +
  theme_bw() +
  theme(axis.text.x=element_text(size=8),
        axis.text.y=element_text(size=12),
        plot.margin = margin(t = 15,
                             r = 15,
                             b = 15,
                             l = 15),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size = 13),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), size = 13),
        strip.background = element_rect(fill = "white", color = "black"),
        strip.text = element_text(size = 12)  # style for facet labels
  )








