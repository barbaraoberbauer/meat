#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# produced under R version: 2024.09.0
#---

# function for calculating mean and median of subjects posterior distributions

parameter_recovery_subjectParameters <- function(combined_mcmcfin, pattern){
  
  # Select columns that match any pattern
  cols_to_keep <- grepl(pattern, colnames(combined_mcmcfin))
  
  # Subset the data frame
  subset <- combined_mcmcfin[, cols_to_keep]
  
  # Compute means and medians
  means <- colMeans(subset)
  medians <- apply(subset, 2, median)
  col_names <- names(means)
  
  subj_parameters <- as.data.frame(rbind(means, medians))
  return(subj_parameters)
  
}