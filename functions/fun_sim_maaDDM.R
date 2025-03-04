#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# last update: "2025-03-03"
# produced under R version: 2024.09.0
#---

# function for simulating data using rtdists

sim_maaDDM <- function(N,
                       Subject,
                       Session,
                       Price_Eco,
                       Energy_Eco,
                       Popularity_Eco,
                       Price_NonEco,
                       Energy_NonEco,
                       Popularity_NonEco,
                       fixProps,
                       
                       # parameters
                       w1T, 
                       w1T_AT,
                       w2T,
                       w2T_AT,
                       w3T,
                       w3T_AT, 
                       thetaT,
                       thetaT_AT, 
                       phiT, 
                       phiT_AT, 
                       alpha, 
                       alpha_AT, 
                       scaling, 
                       scaling_AT, 
                       tau, 
                       tau_AT,
                       sp,
                       sp_AT) {
  
  # initiate data frame to store simulated rts
  sim_rt <- data.frame(matrix(nrow=N,
                              ncol = 1))
  colnames(sim_rt) <- "rt"
  
  for (i in 1:N) {
    
    #specify parameters depending on sessions
    weight1 <- w1T[Subject[i]]*setequal(Session[i],1) + w1T_AT[Subject[i]]*setequal(Session[i],2)
    weight2 <- w2T[Subject[i]]*setequal(Session[i],1) + w2T_AT[Subject[i]]*setequal(Session[i],2)
    weight3 <- w3T[Subject[i]]*setequal(Session[i],1) + w3T_AT[Subject[i]]*setequal(Session[i],2)
    
    theta_x <- thetaT[Subject[i]]*setequal(Session[i],1) + thetaT_AT[Subject[i]]*setequal(Session[i],2)
    
    phi_x <- phiT[Subject[i]]*setequal(Session[i],1) + phiT_AT[Subject[i]]*setequal(Session[i],2)
    
    alpha_x <- alpha[Subject[i]]*setequal(Session[i],1) + alpha_AT[Subject[i]]*setequal(Session[i],2)
    
    scaling_x <- scaling[Subject[i]]*setequal(Session[i],1) + scaling_AT[Subject[i]]*setequal(Session[i],2)
    
    tau_x <- tau[Subject[i]]*setequal(Session[i],1) + tau_AT[Subject[i]]*setequal(Session[i],2)
    
    sp_x <- sp[Subject[i]]*setequal(Session[i],1) + sp_AT[Subject[i]]*setequal(Session[i],2)
    
    
    # calculate mean drift rate mu
    mu <- 	
      fixProps[i,1]*(weight1*(Price_Eco[i]-theta_x*Price_NonEco[i]) + weight2*phi_x*(Energy_Eco[i]-theta_x*Energy_NonEco[i]) + weight3*phi_x*(Popularity_Eco[i]-theta_x*Popularity_NonEco[i])) + # Price_Eco is looked at
      fixProps[i,2]*(weight1*phi_x*(Price_Eco[i]-theta_x*Price_NonEco[i]) + weight2*(Energy_Eco[i]-theta_x*Energy_NonEco[i]) + weight3*phi_x*(Popularity_Eco[i]-theta_x*Popularity_NonEco[i])) + # Consumption_Eco is looked at
      fixProps[i,3]*(weight1*phi_x*(Price_Eco[i]-theta_x*Price_NonEco[i]) + weight2*phi_x*(Energy_Eco[i]-theta_x*Energy_NonEco[i]) + weight3*(Popularity_Eco[i]-theta_x*Popularity_NonEco[i])) + # Popularity_Eco is looked at
      fixProps[i,4]*(weight1*(theta_x*Price_Eco[i]-Price_NonEco[i]) + weight2*phi_x*(theta_x*Energy_Eco[i]-Energy_NonEco[i]) + weight3*phi_x*(theta_x*Popularity_Eco[i]-Popularity_NonEco[i])) + # Price_NonEco is looked at
      fixProps[i,5]*(weight1*phi_x*(theta_x*Price_Eco[i]-Price_NonEco[i]) + weight2*(theta_x*Energy_Eco[i]-Energy_NonEco[i]) + weight3*phi_x*(theta_x*Popularity_Eco[i]-Popularity_NonEco[i])) + # Consumption_NonEco is looked at
      fixProps[i,6]*(weight1*phi_x*(theta_x*Price_Eco[i]-Price_NonEco[i]) + weight2*phi_x*(theta_x*Energy_Eco[i]-Energy_NonEco[i]) + weight3*(theta_x*Popularity_Eco[i]-Popularity_NonEco[i]))   # Popularity_NonEco is looked at
    
    # draw from rt distribution
    result <- rdiffusion(1, alpha_x, scaling_x * mu, tau_x, sp_x * alpha_x) # starting point is absolute in rtdists
    
    # return response time (+ if eco choice, i.e. upper, - if non-eco choice, i.e. lower)
    rt <- if (result$response == "upper") {result$rt} else {-1 * result$rt}
    
    # store in data frame
    sim_rt[i, 1] <- rt
    
  }
  
  # return data frame
  return(sim_rt)
  
}


