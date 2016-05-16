# OBS! Fra dette skridt bruger de i estimationen kun de 3 indkomstgrupper
# de gerne vil finde MWP fra

## Estimation step 3 prep - Recoveringm values and prep for getting back valuef

gamma <- c(-9.5, 0.02, 0.1, -0.2, 0.006) # will be given by former estimations

#Splitting gamma into FMC, PMC and a moving cost time trend coefficient

gamma.pmc    <- gamma[1:2]
gamma.fmc    <- gamma[4:5]
mc.timetrend <- gamma[3]

# We retrieve the mtau values and time invarient moving costs
gammafmctau   <- rep(0,n.types)
m.tau   <- rep(0,n.types)
pmc.inv <- matrix(0, n.types ,1)

for (m in 1:n.types){
  
  gammafmctau[m] <- gamma.fmc[1] + gamma.fmc[2] * type.comb[m, "income"]
  
  m.tau[m]   <- gammafmctau[m] * type.comb[m, "wealth"]
  
  pmc.inv[m] <- (gamma.pmc[1] * type.comb[m,"wealth"] + gamma.pmc[2] * type.comb[m, "income"])
                                                                                 
}

## Obs Her skal besluttes vorvidt vi ønsker at bruge alle typeværdier
## eller trække enkelte ud af sættet til MWP regressionerne

# Simulate Nhood matrix
df.crime     <- matrix(0,n.neighborhoods,n.periods)
df.pollution <- matrix(0,n.neighborhoods,n.periods)

for (j in 1:n.neighborhoods) {
  for (t in 1:n.periods){
   if (t == 1) {
     
     df.pollution[j, t] = runif(1,1,10)
     df.crime[j,t]     = runif(1,10,1000)
     
   } else {
     
     df.pollution[j, t] = df.pollution[j, t - 1] + rnorm(1,0,2) 
     df.crime[j, t]     = df.crime[j, t - 1] + rnorm(1,0,100)
     
   }
  }
}

# Reshape to vectors containing all Nhoods in sequential times

crime.vec <- as.matrix(as.vector(df.crime))
poll.vec  <- as.matrix(as.vector(df.pollution))

# Create time and nhoods dummies
n.lags = 2

dummy <- rep(c(1:n.neighborhoods), n.periods)
dummy <- dummy(dummy[(n.lags * n.neighborhoods + 1):length(dummy)])

time = dummy
dummy.time <- c((n.lags + 1):n.periods)
dummy.time <- time * as.vector(kronecker(dummy.time, rep(1, n.neighborhoods)))
    
# Find the mean price level for all years

meanprices <- data.frame(matrix(0,nrow = n.neighborhoods, ncol = n.periods))

for (t in 1:n.periods) {
  for (j in 1:n.neighborhoods) {
    
    temp <- subset(zdata, year.ind == t & nhood == j)
    meanprices[j,t] = mean(temp$price)
    
  }
}







