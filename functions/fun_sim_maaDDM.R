#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# last update: "2025-03-03"
# produced under R version: 2024.09.0
#---

# function for simulating data using rtdists

sim_maaDDM <- function(parameters,
                       Price_Eco,
                       Price_NonEco,
                       Energy_Eco,
                       Energy_NonEco,
                       Popularity_Eco,
                       Popularity_NonEco,
                       fixProps
                       ) {
  
  alpha <- parameters[1] # boundary separation
  tau <- parameters[2] # non-decision time
  scaling <- parameters[3] 
  theta <- parameters[4] 
  phi <- parameters[5] 
  weight1 <- parameters[6] # weight price
  weight2 <- parameters[7] # weight consumption
  weight3 <- parameters[8] # weight popularity
  sp <- parameters[9] * alpha # starting point bias; relative in dwiener but absolute in rtdists
  
  # compute drift rate
  v <- 
    fixProps[,1]*(weight1*(Price_Eco-theta*Price_NonEco) + weight2*phi*(Energy_Eco-theta*Energy_NonEco) + weight3*phi*(Popularity_Eco-theta*Popularity_NonEco)) + # Price_Eco is looked at
    fixProps[,2]*(weight1*phi*(Price_Eco-theta*Price_NonEco) + weight2*(Energy_Eco-theta*Energy_NonEco) + weight3*phi*(Popularity_Eco-theta*Popularity_NonEco)) + # Consumption_Eco is looked at
    fixProps[,3]*(weight1*phi*(Price_Eco-theta*Price_NonEco) + weight2*phi*(Energy_Eco-theta*Energy_NonEco) + weight3*(Popularity_Eco-theta*Popularity_NonEco)) + # Popularity_Eco is looked at
    fixProps[,4]*(weight1*(theta*Price_Eco-Price_NonEco) + weight2*phi*(theta*Energy_Eco-Energy_NonEco) + weight3*phi*(theta*Popularity_Eco-Popularity_NonEco)) + # Price_NonEco is looked at
    fixProps[,5]*(weight1*phi*(theta*Price_Eco-Price_NonEco) + weight2*(theta*Energy_Eco-Energy_NonEco) + weight3*phi*(theta*Popularity_Eco-Popularity_NonEco)) + # Consumption_NonEco is looked at
    fixProps[,6]*(weight1*phi*(theta*Price_Eco-Price_NonEco) + weight2*phi*(theta*Energy_Eco-Energy_NonEco) + weight3*(theta*Popularity_Eco-Popularity_NonEco))   # Popularity_NonEco is looked at
  
  # simulate rts
  nTrials <- length(v)
  choices <- rep(NA, nTrials)
  rts <- rep(NA, nTrials)
  
  for (i in 1:nTrials) {
    result <- rdiffusion(1, alpha, scaling * v[i], tau, sp)
    choices[i] <- ifelse(result$response == "upper", 1, 0)
    rts[i] <- result$rt
  }
  
  # bind data
  sim_results <- cbind(choices, rts)
  
  return(sim_results)
  
}