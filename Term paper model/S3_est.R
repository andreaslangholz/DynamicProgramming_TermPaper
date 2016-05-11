

# The development in valuefunctions over time per type conditional on the development of amenities
# is found by regression

beta.m <- as.data.frame(matrix(0, 16, n.types))
res.m  <- as.data.frame(matrix(0, (n.periods - n.lags) * n.neighborhoods, n.types))

beta.o <- as.data.frame(matrix(0, 2, 9))
res.o  <- as.data.frame(matrix(0, 8, n.types))

beta.price <- as.data.frame(matrix(0, 16, 1)) 
res.price  <- as.data.frame(matrix(0, (n.periods-n.lags)*n.neighborhoods, 1)) 

# Regressing the value evolvement for each type over time
for (m in 1:n.types) {
  
  val.vec <- as.matrix(as.vector(value.functions.tilde[m,1:n.neighborhoods, ]))
  temp <- as.matrix(cbind(val.vec, crime.vec, poll.vec))
  lag1 <- rbind(matrix(0, nrow = n.neighborhoods,3), temp[1:(nrow(temp) - n.neighborhoods), ])
  lag2 <- rbind(matrix(0, nrow = (n.neighborhoods * 2), 3), temp[1:(nrow(temp) - n.neighborhoods * 2), ])
  lags <- cbind(lag1, lag2)

  Y <- temp[(2 * n.neighborhoods + 1):nrow(temp), 1]
  X <- cbind(dummy, dummy.time, lags[(2 * n.neighborhoods + 1):nrow(lags), ])
  
  beta.m[, m] <- solve(t(X) %*% X) %*% t(X) %*% Y
  res.m[, m]  <- Y - X %*% beta.m[ ,m]

# We find the parameters for the outside option, as we do not have other regressors than the valuefunctions
  
  val.vec.out = as.matrix(as.vector(value.functions.tilde[m, n.neighborhoods + 1, ]))
  
  lag1 <- rbind(matrix(0, 1, 1), as.matrix(val.vec.out[1:(nrow(val.vec.out) - 1), ]))
  lag2 <- rbind(matrix(0, 2, 1), as.matrix(val.vec.out[1:(nrow(val.vec.out) - 2), ]))
  lags <- cbind(lag1, lag2)
  
  Y <- val.vec.out[(2 + 1):nrow(val.vec.out), 1]
  X <- cbind(lags[(2 + 1):nrow(val.vec.out), ])
  
  beta.o[, m] <- solve(t(X) %*% X) %*% t(X) %*% Y
  res.o[, m]  <- Y - X %*% beta.o[, m]
}

# Regressing the evolvement of prices as a function of Nhood amenities ----

price.vec <- as.matrix(as.vector(as.matrix(meanprices[1:n.neighborhoods, ])))
temp.t    <- as.matrix(cbind(price.vec, crime.vec, poll.vec))
lag1      <- rbind(matrix(0,nrow = n.neighborhoods, 3), temp.t[1:(nrow(temp.t) - n.neighborhoods), ])
lag2      <- rbind(matrix(0,nrow = (n.neighborhoods * 2), 3), temp.t[1:(nrow(temp.t) - n.neighborhoods * 2), ])
lags      <- cbind(lag1, lag2)

Y <- temp.t[(2 * n.neighborhoods + 1):nrow(temp.t), 1]
X <- cbind(dummy, dummy.time, lags[(2 * n.neighborhoods + 1):nrow(lags), ])

beta.price[, 1] <- ginv(t(X) %*% X) %*% t(X) %*% Y
res.price[, 1]  <- Y - X %*% beta.price[, 1]

### Preparing simulation of the expected value functions based on regressions -----

# Number of draws for integration
R = 1000

# Splitting coefficients from regressions

# Kappa is the valuefunction coefficients for the lags, nhood and time trend dummies
kappa0 <- beta.m[1:n.neighborhoods, ]
kappa1 <- beta.m[(n.neighborhoods + 1):(2 * n.neighborhoods), ]

# Alpha is the  valuefunction coefficients  for crime and pollution amenities with lags
alpha11 <- beta.m[(2 * n.neighborhoods + 1):(2 * n.neighborhoods + 3), ]
alpha12 <- beta.m[(2 * n.neighborhoods + 3):(2 * n.neighborhoods + 6), ]

# Omega is the price coefficients for the nhood and time trend dummies
omega0 <- beta.price[1:n.neighborhoods, ]
omega1 <- beta.price[(n.neighborhoods + 1):(2 * n.neighborhoods), ]

# gamma is the price coefficients for the amenities 
gamma11 <- beta.price[(2 * n.neighborhoods + 1):(2 * n.neighborhoods + 3), ]
gamma12 <- beta.price[(2 * n.neighborhoods + 3):(2 * n.neighborhoods + 6), ]









