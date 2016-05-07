
EstimateValuefunctionsTilde <- function(shares, n.types, n.neighborhoods, n.periods){
  
  #  Calculates the estimated valuefunctions based on the FOC of MLE. Each valuefunction equals the difference in moving probability
  #  minus the average propensity to move to a specific neighborhood for a given type of household. 
  #  Equation (18) in paper. 
  #
  #Args :
  #  shares: the share of households moving
  #  n.types
  
  
  value.tilde <- array(rep(NaN, n.types * (n.neighborhoods + 1) * t), c(n.types, (n.neighborhoods + 1), n.periods))
  
  for (m in 1:n.types){
    
    for (t in 1:n.periods){
      
      value.tilde[m, , t] <- log(shares[m, , t]) - mean(log(shares[m, ,t]))
      
    }
  }
  
  return(value.tilde)
  
}


LikelihoodOfStayDecision <- function(y, x, value.stay, value.move, initial.param, max.iteration, tolerance){
  
  b = initial.param
  crit = 1
  nr.vars = length(b) 
  
  #  Calculate the CCP of staying in residence - eq (15)
  ccp.stay = value.stay / (value.stay + value.move * exp(x %*% b)) 
  
  #  Take the gradient of the likelihood 
  gradient = (y - ccp.stay) %*% x
  
  pdf = ccp.stay * (1 - ccp.stay)
  
  h = matrix(data = rep(1, nr.obs * nr.vars), nrow = nr.obs, ncol = nr.vars)
  
  hessian = - t((pdf * h) * x) %*% x 
  
  hessian.inv = ginv(hessian)
  
  newton.raps.coef = - hessian.inv %*% t(gradient)
  
}