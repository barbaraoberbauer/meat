#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# produced under R version: 2024.09.0
#---

# function for retrieving hdis of relevant parameters

parameter_recovery_hdis <- function(combined_mcmcfin){
  
  hdi <- list()
  
  combined_mcmcfin <- combined_mcmcfin
  
  # boundary separation
  hdi$alpha <- list(hdi_baseline = HDIofMCMC(combined_mcmcfin$mu_alpha),
                    hdi_manipulation = HDIofMCMC(combined_mcmcfin$mu_alpha + combined_mcmcfin$mu_dalpha),
                    hdi_change = HDIofMCMC(combined_mcmcfin$mu_dalpha))
  
  # price 
  
  hdi$w_price <- list(hdi_baseline = HDIofMCMC(pnorm(combined_mcmcfin$mu_w1)),
                      hdi_manipulation = HDIofMCMC(pnorm(combined_mcmcfin$mu_w1 + combined_mcmcfin$mu_dw1)),
                      hdi_change = HDIofMCMC(pnorm(combined_mcmcfin$mu_w1 + combined_mcmcfin$mu_dw1) -
                                               pnorm(combined_mcmcfin$mu_w1)))
  
  # consumption 
  
  hdi$w_consumption <- list(hdi_baseline = HDIofMCMC(pnorm(combined_mcmcfin$mu_w2)),
                            hdi_manipulation = HDIofMCMC(pnorm(combined_mcmcfin$mu_w2 + combined_mcmcfin$mu_dw2)),
                            hdi_change = HDIofMCMC(pnorm(combined_mcmcfin$mu_w2 + combined_mcmcfin$mu_dw2) -
                                                     pnorm(combined_mcmcfin$mu_w2)))
  
  # theta
  hdi$theta <- list(hdi_baseline = HDIofMCMC(combined_mcmcfin$mu_theta),
                    hdi_manipulation = HDIofMCMC(combined_mcmcfin$mu_theta + combined_mcmcfin$mu_dtheta),
                    hdi_change = HDIofMCMC(combined_mcmcfin$mu_dtheta))
  
  # phi
  hdi$phi <- list(hdi_baseline = HDIofMCMC(combined_mcmcfin$mu_phi),
                  hdi_manipulation = HDIofMCMC(combined_mcmcfin$mu_phi + combined_mcmcfin$mu_dphi),
                  hdi_change = HDIofMCMC(combined_mcmcfin$mu_dphi))
  
  # scaling  
  
  hdi$scaling <- list(hdi_baseline = HDIofMCMC(combined_mcmcfin$mu_scaling),
                      hdi_manipulation = HDIofMCMC(combined_mcmcfin$mu_scaling + combined_mcmcfin$mu_dscaling),
                      hdi_change = HDIofMCMC(combined_mcmcfin$mu_dscaling))
  
  
  # non-decision time  
  
  hdi$tau <- list(hdi_baseline = HDIofMCMC(combined_mcmcfin$mu_tau),
                  hdi_manipulation = HDIofMCMC(combined_mcmcfin$mu_tau + combined_mcmcfin$mu_dtau),
                  hdi_change = HDIofMCMC(combined_mcmcfin$mu_dtau))
  
  
  # starting point bias  
  
  hdi$sp <- list(hdi_baseline = HDIofMCMC(combined_mcmcfin$mu_sp),
                 hdi_manipulation = HDIofMCMC(combined_mcmcfin$mu_sp + combined_mcmcfin$mu_dsp),
                 hdi_change = HDIofMCMC(combined_mcmcfin$mu_dsp))
  
  return(hdi)
  
}
