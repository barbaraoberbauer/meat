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
                                size = base_size + 6,
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

