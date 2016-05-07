
EstimateValuefunctionsTilde <- function(shares, n.types, n.neighborhoods, n.periods){
  
  #  Calculates the estimated valuefunctions based on the FOC of MLE. Each valuefunction equals the difference in moving probability
  #  minus the average propensity to move to a specific neighborhood for a given type of household. 
  #  Equation (18) in paper. 
  #
  #Args :
  #  shares: the share of households moving
  #  n.types
  
  cells = n.types * (n.neighborhoods + 1) * n.periods
  
  value.tilde <- array(rep(NaN, cells), c(n.types, (n.neighborhoods + 1), n.periods))
  
  for (m in 1:n.types){
    
    for (t in 1:n.periods){
      
      value.tilde[m, , t] <- log(shares[m, , t]) - mean(log(shares[m, ,t]))
      
    }
  }
  
  return(value.tilde)
  
}


LikelihoodOfStayDecision <- function(y, x, value.stay, value.move, initial.param, max.iteration, tolerance){
  
  b = initial.param
  criterion = 1
  iteration = 1
  
  nr.vars = length(b) 
  
  #  Looping over successive ML approximations by the Newton Rhapson algorithm
  while (criterion > tolerance & iteration < max.iteration) {
      
    
    #  Calculate the CCP of staying in residence - eq (15)
    ccp.stay = value.stay / (value.stay + value.move * exp(x %*% b)) 
    
    #  Take the gradient of the likelihood 
    gradient = t(y - ccp.stay) %*% x
    
    #  Take the pdf of the choice probabilities which enters into the Hessian
    ccp.pdf = ccp.stay * (1 - ccp.stay)
    
    w = matrix(data = rep(NaN, nr.obs * nr.vars), nrow = nr.obs, ncol = nr.vars)
    
    for (i in 1:nr.vars){
      
      w[, i] = ccp.pdf
      
    }
    
    #  Calculate the Hessian 
    hessian = - t(w * x) %*% x
    
    hessian.inv = ginv(hessian)
    
    # Residual in the Newton - Rhapson procedure
    newton.raps.res = - hessian.inv %*% t(gradient)
    
    # Newton-Rhapson optimization
    step.size = 2
    
    likelihood.a = 0
    likelihood.b = 1
    
    while (likelihood.b > likelihood.a) {
      
      step.size = step.size / 2
      
      b.tilde.a = b + step.size * newton.raps.res
      b.tilde.b = b + step.size * newton.raps.res / 2
      
      likelihood.a = LikelihoodLogitNR(b.tilde.a, y, x, value.stay, value.move)
      likelihood.b = LikelihoodLogitNR(b.tilde.b, y, x, value.stay, value.move)
      
    }
    
    b.tilde = b + step.size * newton.raps.res
    b = b.tilde
    
    criterion = max(newton.raps.res)
    iteration = iteration + 1
  
  }
  
  likelihood = LikelihoodLogitNR(b, y, x, value.stay, value.move)  
  
  ouput = list(b, likelihood, iteration, criterion)  
  
  return(output)
}


LikelihoodLogitNR <- function(b.tilde, y, x, value.stay, value.move ){
  #  Helper function for LikelihoodOfStayDecision to compute the 
  #  likelihood in each step of the Newton Rhapson algorithm
  # 
  #  Args: 
  #  b.tilde: beta parameters with added NR residual  
  
  ccp.stay = value.stay / (value.stay + value.move * exp(x %*% b.tilde))
  
  likelihood.contribution = y * log(ccp.stay) + (1 - y) * log(1 - ccp.stay)
  
  likelihood = sum(likelihood.contribution)
  
  return(likelihood)
  
}

