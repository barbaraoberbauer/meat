#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# last update: "2025-01-23"
# produced under R version: 2024.09.0
#---

# function for normalizing attribute values
# Berkowitsch et al., 2015: https://bpspsychub.onlinelibrary.wiley.com/doi/10.1111/bmsp.12048

#function for normalization 
normalizeValue <- function(v_old, min_old, max_old, min_new, max_new){
  
  v_new <- min_new + (v_old - min_old)*(max_new - min_new)/(max_old - min_old)
  return(v_new)
  
}
