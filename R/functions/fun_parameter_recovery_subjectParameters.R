#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# produced under R version: 2024.09.0
#---

# function for calculating mean and median of subjects posterior distributions

parameter_recovery_subjectParameters <- function(combined_mcmcfin, pattern, sim){
  
  # Select columns that match any pattern
  cols_to_keep <- grepl(pattern, colnames(combined_mcmcfin))
  
  # Subset the data frame
  subset <- combined_mcmcfin[, cols_to_keep]
  
  # Compute means and medians
  means <- colMeans(subset)
  medians <- apply(subset, 2, median)
  col_names <- names(means)
  
  # Transpose data frame
  subj_parameters <- as.data.frame(rbind(means, medians))
  subj_parameters <- as.data.frame(t(subj_parameters))
  
  # Shape into long format
  subj_parameters <- subj_parameters %>%
    mutate(parameter_subject = rownames(.)) %>%
    as.data.frame()
  rownames(subj_parameters) <- NULL
  
  subj_parameters <- subj_parameters %>%
    mutate(
      parameter = str_extract(parameter_subject, "^\\w+"),
      subject = str_extract(parameter_subject, "\\d+(?=\\])") %>% 
        as.integer()
    ) %>%
    select(-parameter_subject)

  # Add sim num
  subj_parameters$sim <- sim
  
  return(subj_parameters)
  
}
