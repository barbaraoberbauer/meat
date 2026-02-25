#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# produced under R version: 2024.09.0
#---

# function for retrieving hdis of relevant parameters

parameter_recovery_hdis <- function(combined_mcmcfin, sim){
  
  # Set data
  combined_mcmcfin <- combined_mcmcfin
  
  # Initialize data frame containing all relevant parameters
  parameters <- c("mu_w1", 
                  "mu_dw1", 
                  "mu_w2", 
                  "mu_dw2", 
                  "mu_theta", 
                  "mu_dtheta", 
                  "mu_phi", 
                  "mu_dphi", 
                  "mu_alpha", 
                  "mu_dalpha", 
                  "mu_scaling", 
                  "mu_dscaling", 
                  "mu_tau", 
                  "mu_dtau", 
                  "mu_sp", 
                  "mu_dsp")
  
  df <- data.frame(parameter = parameters)
  
  # Add info about simulation number
  df$sim <- sim
  
  # Add hdi upper and lower
  df$hdi_lower <- NA
  df$hdi_upper <- NA
  
  # Set hdi's
  for (i in 1:length(parameters)) {
    param <- parameters[i]
      # possibly pnorm phi-transformed parameters
      df[df$parameter == param, "hdi_lower"] <- HDIofMCMC(combined_mcmcfin[[param]])[1]
      df[df$parameter == param, "hdi_upper"] <- HDIofMCMC(combined_mcmcfin[[param]])[2]
  }
  
  return(df)
  
  
}
