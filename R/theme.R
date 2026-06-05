#'---------------------------------
#
# Description: Set theme for plots in meat
# Author: barbara.oberbauer@uni-hamburg.de

#'---------------------------------

# plot theme
themeMEAT <- function(base_size = 12) {
  theme_bw(base_size = base_size) +
    theme(
      # title
      plot.title = element_text(hjust = 0.5,
                                size = base_size + 8,
                                face = "bold",
                                margin = margin(t = 0,
                                                r = 0,
                                                b = 15,
                                                l = 0)),
      
      # axes
      axis.title = element_text(margin = margin(t = 0, 
                                                r = 15, 
                                                b = 0, 
                                                l = 0),
                                size = base_size + 2),
      axis.title.x = element_text(margin = margin(t = 15, 
                                                  r = 0, 
                                                  b = 0, 
                                                  l = 0)),
      axis.title.y = element_text(margin = margin(t = 0, 
                                                 r = 15, 
                                                 b = 0, 
                                                 l = 0)),
      axis.text  = element_text(size = base_size - 2),
      axis.line = element_line(colour = "black"),
      
      # legend
      legend.text = element_text(size = base_size + 4),
      legend.title = element_text(size = base_size + 4),
      legend.key.size = unit(0.5, 
                             "cm"),
      legend.position = "top",
      
      # panel
      panel.border = element_blank(), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      
      # plot maring
      plot.margin = margin(t = 15,
                           r = 15,
                           b = 15,
                           l = 15)
      
    )
}

# colors sessions
color_sessions <- c("#225780", "#8CC5E3")

# colors price translation (only relevant for supplements)
color_price <- c("#2D3142", "#BFC0C0") # present, absent

# colors choice
color_choice <- c("#CBCBD4", "#556F44") # non-eco, eco choice

# colors error
color_error <- '#cb181d'

# colors change
color_change <-"black" 

# colors gender 
color_gender <- c("#D4CB92", "#395C6B")

# colors attribute
#color_attributes <- c("#EB821A", "#6BA89C", "#23373B")

color_attributes <- c("#B8A0D4", "#D4457A", "#4A2070")

# Option 1: Light lavender, bright pink, deep purple
c("#B8A0D4", "#D4457A", "#4A2070")

# Option 2: Soft lilac, hot pink, dark plum
c("#C4A8E0", "#C42080", "#3D1455")

# Option 3: Periwinkle, magenta, dark berry
c("#8E82C9", "#D4308A", "#5C1A6B")

# Option 4: Pale lavender, vivid magenta, near-black purple
c("#C0A8E8", "#C41E8A", "#2D0A45")

# Option 5: Mid purple, hot pink, dark wine
c("#7B52B8", "#E0208A", "#4A0A3A")


# labels
labelsOriginal <- c("control" = "Control",
                    "operating_costs" = "Operating\nCosts",
                    "emissions" = "Carbon\nEmissions",
                    "environmental_friendliness" = "Rating")

labelsReplication <- c("control" = "Control",
                       "emission_replace" = "Carbon \nEmissions \nReplace",
                       "rating_replace" = "Rating \nReplace",
                       "emission_add" = "Carbon \nEmissions \nAdd",
                       "rating_add" = "Rating \nAdd")

labelsAttributesT <- c("t_price0" = "Price\nOther",
                      "t_price1" = "Price\nEco",
                      "t_consumption0" = "Consumption\nOther",
                      "t_consumption1" = "Consumption\nEco",
                      "t_popularity0" = "Popularity\nOther",
                      "t_popularity1" = "Popularity\nEco")

labelsAttributesF <- c("f_price0" = "Price\nOther",
                       "f_price1" = "Price\nEco",
                       "f_consumption0" = "Consumption\nOther",
                       "f_consumption1" = "Consumption\nEco",
                       "f_popularity0" = "Popularity\nOther",
                       "f_popularity1" = "Popularity\nEco")

labelsDwellTimeProportions <- c("difFixPropPrice" = "Price",
                                "difFixPropConsumption" = "Consumption",
                                "difFixPropPopularity" = "Popularity")



