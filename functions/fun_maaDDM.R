#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# last update: "2025-01-23"
# produced under R version: 2024.09.0
#---

# function for running the multi-attribute attentional DDM (Yang & Krajbich, 2023)

maaDDM <- function(dat, monitor, model_file, model_specifications) {
  
  # set up cluster manually and make sure module is loaded before running the model
  # https://sourceforge.net/p/runjags/forum/general/thread/e34ce49c3c/ 
  cl <- makePSOCKcluster(nchains)
  clusterCall(cl, function(x) require("wiener"))
  
  runJagsOut <- run.jags(method = "parallel",
                         model = model_file,
                         monitor = monitor,
                         module = "wiener",
                         data = dat,
                         n.chains = model_specifications$nchains,
                         #inits = inits,
                         adapt = model_specifications$nAdaptSteps,
                         burnin = model_specifications$nBurninSteps,
                         sample = ceiling(model_specifications$nUseSteps/model_specifications$nchains),
                         thin = nThinSteps,
                         summarise = TRUE,
                         plots = FALSE)
  
  return(runJagsOut)
    
}