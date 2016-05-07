

library(tidyr)
library(dplyr)

zdata = read.csv("C:\\Users\\Andreas\\Documents\\GitHub\\DynamicProgramming_TermPaper\\simuleret data.csv", header = TRUE, sep = ",")

# parameters & Initial values -----------------------------

used.areas = c(1:5) # meaning that in the simulated data, option 6 will be outside the

n.periods <- length(unique(zdata$current.year))

neighboorhood <- unique(zdata$area)

n.neighborhoods <- length(used.areas)

n.obs <- nrow(zdata)

moving.costs <- 0.06  # omkostningerne ved at sælge ( undersøg om det er de samme i DK)

years <- unique(zdata$current.year)

zdata$outside <- ifelse(zdata$flyt == 1 & zdata$kom.t.1 != used.areas, 1, 0)
zdata$inside  <- ifelse(zdata$flyt == 1 & zdata$kom.t.1 == used.areas, 1, 0)

# indicators for years and neighborhoods

for (i in 1:n.obs) {
 zdata$year.ind[i] = which(years == zdata$current.year[i])
}

for(i in 1:n.obs) {
  zdata$nhood[i] = which(neighboorhood == zdata$area[i])
}

# Splitting observations in types based on income and wealth ----------------------
n.incometypes = 2
n.wealthtypes = 2

income.max = max(zdata$income)
income.min = min(zdata$income)
income.bins = seq(income.min, income.max, income.max / n.incometypes)
income.bins[n.incometypes + 1] <- Inf                                # Det maksimale loft for indtægt i den sidste gruppe er uendeligt

wealth.max = max(zdata$wealth)
wealth.min = min(zdata$wealth)
wealth.bins = seq(wealth.min, wealth.max, wealth.max / n.wealthtypes)
wealth.bins[n.wealthtypes +1 ] = Inf                                 # det maksimale loft for formue i det sidste led er uendeligt

n.types <- n.wealthtypes * n.incometypes

type.matrix <- as.data.frame(matrix(1:n.types,n.wealthtypes,n.incometypes))

for (i in 1:n.obs) {
  for (j in 1:n.incometypes) {
    for (w in 1:n.wealthtypes) {
      
      if(zdata$income[i] >= income.bins[j] & zdata$income[i] < income.bins[j + 1]
       & zdata$wealth[i] >= wealth.bins[w] & zdata$wealth[i] < wealth.bins[w + 1]) zdata$type.tau[i] <- type.matrix[w,j]
        
  }
 } 
}


# New number of types, as we cant loop over types without content ## OBS Tjek lige om det her step er korrekt

n.types <- length(unique(zdata$type.tau))

# Creating the Conditional Choice Probabilities by splittingon time, neighborhoods and years -------------------

# Creating frequency tables for observations of each type/year/hood combination and moving decisions

# nr in each combination

group.obs <- tbl_df(zdata) %>%
  group_by(type.tau, year.ind, flyt) %>% 
  summarise(nr = n())

fact.types <- c("type.tau","year.ind", "flyt", "nr")

group.obs <- as.data.frame(lapply(group.obs[fact.types], as.factor))

full.grid = expand.grid(type.tau = levels(as.factor(zdata$type.tau)), year.ind = levels(as.factor(zdata$year.ind)), 
                        flyt = levels(as.factor(zdata$flyt)))

group.obs <- left_join(full.grid,group.obs)

group.obs$nr <- as.numeric(group.obs$nr)

group.obs$nr[is.na(group.obs$nr)] <- 0

# nr of each combination whith movement

group.obs.move <- tbl_df(zdata) %>%
  group_by(type.tau, year.ind, kom.t.1, flyt) %>% 
  summarise(nr = n())

fact.types <- c("type.tau","year.ind","flyt","kom.t.1", "nr")

group.obs.move <- as.data.frame(lapply(group.obs.move[fact.types], as.factor))

full.grid = expand.grid(type.tau = levels(as.factor(zdata$type.tau)), year.ind = levels(as.factor(zdata$year.ind)),
                        kom.t.1 = levels(as.factor(zdata$kom.t.1)), flyt = levels(as.factor(zdata$flyt)))

group.obs.move <- left_join(full.grid,group.obs.move)

group.obs.move$nr <- as.numeric(group.obs.move$nr)

group.obs.move$nr[is.na(group.obs.move$nr)] <- 0

# nr of each combination moving outside the chosen areas

group.obs.move.out <- tbl_df(zdata) %>%
  group_by(type.tau, year.ind, outside) %>% 
  summarise(nr = n())

fact.types <- c("type.tau","year.ind", "nr", "outside")

group.obs.move.out <- as.data.frame(lapply(group.obs.move.out[fact.types], as.factor))

full.grid = expand.grid(type.tau = levels(as.factor(zdata$type.tau)), year.ind = levels(as.factor(zdata$year.ind)), 
                        outside = levels(as.factor(zdata$outside)))

group.obs.move.out <- left_join(full.grid, group.obs.move.out)

group.obs.move.out$nr <- as.numeric(group.obs.move.out$nr)

group.obs.move.out$nr[is.na(group.obs.move.out$nr)] <- 0

# Calculating the % share of move/stay for each type/year/hood combination (man kunne overveje et ekstra if statement for at komme af NaN)

shares.moving <- array(0,dim=c(n.types,n.neighborhoods + 1, n.periods))

# Findind CCP's of moving to neighborhood t conditional on moving

for (t in 1:n.periods) {
  for (m in 1:n.types) {
     for (j in 1:n.neighborhoods) {
       
       sum.move <- group.obs$nr[(group.obs$type.tau == m) & (group.obs$year.ind == t) & (group.obs$flyt == 1)] 
       sum.tau <- group.obs.move$nr[(group.obs.move$kom.t.1 == j) & (group.obs.move$flyt == 1) 
                                    & (group.obs.move$type.tau == m) & (group.obs.move$year.ind == t)] 
                                         
       if (length(sum.tau / sum.move) == 0) {
         shares.moving[m,j,t] = 0.0000001
       }
       
       else if (sum.move == 0 & sum.tau > 0) {
         shares.moving[m,j,t] = 1
       }
       else if (sum.tau == 0){
         shares.moving[m,j,t] = 0.0000001
       }
       
       else if(sum.move == 0 & sum.tau == 0){ ### Ikke helt korrekt, men bliver forh�bentligt bedre med flere observationer
         shares.moving[m,j,t] = 0.0000001
       }
       
       else {
         shares.moving[m,j,t] = sum.tau / sum.move
       }
    }
  }
}
# outside shares

for (t in 1:n.periods) {
  for (m in 1:n.types) {
    sum.move <- group.obs$nr[(group.obs$type.tau == m) & (group.obs$year.ind == t) & (group.obs$flyt == 1)]
    sum.out <- group.obs.move.out$nr[(group.obs.move.out$type.tau == m) & group.obs.move.out$year.ind == t 
                                     & (group.obs.move.out$outside == 1)]
    
    if (length(sum.out / sum.move) == 0) {
      shares.moving[m, n.neighborhoods + 1, t] = 0.00000001
    }
    
    else if (sum.move == 0 & sum.out > 0) {
      shares.moving[m, n.neighborhoods + 1 ,t] = 1
    }
    
    else if (sum.tau == 0) {
      shares.moving[m, n.neighborhoods + 1, t] = 0.0000001
    }
    
    else if(sum.move == 0 & sum.out == 0){ ### Ikke helt korrekt, men bliver forh�bentligt bedre med flere observationer
      shares.moving[m, n.neighborhoods + 1 , t] = 0.0000001
    }
    
    else {
      shares.moving[m, n.neighborhoods + 1, t] = sum.out / sum.move
    }
  }
}





shares.moving[,,]

# Creating the ingoing values for the likelihood estimator (the X vector) ---------------------------

# X1 = 1

zdata$x1 = 1

# X2 = income (Psychological Moving Costs)

zdata$x2 = zdata$income

# x3 = 1

zdata$x3 = 1

# x4 = Price_t * moving costs (Financial moving costs)

# First we need to find the potential price in all periods OBS! Her skal laves en variable for husprisindeks afhængig af område

## Midlertidig prisudvikling

stignings.proc = 0.017 #midlertidig fra SIM rapport

stig.vec = array()
for (t in 1:n.periods) {
  stig.vec[t] <- (1+0.017)^t 
}

for (i in 1:n.obs) {
  if (zdata$current.year[i] == min(years)) {
    
    zdata$price[i] = zdata$vurdering[i]
  
    for (t in 1:(T-1)) {
      
      zdata$price[i + t] <- zdata$price[i] * stig.vec[t] 
    }
  }
}

zdata$x4 = zdata$price * moving.costs

# x5 = price * movingcosts * income (Type specific financial moving costs)

zdata$x5 <- zdata$price * moving.costs * zdata$income

# vector of x values for logit

x_sx <- zdata[ ,c("x1","x2","x3","x4","x5")]

y_sx <- zdata[ ,"flyt"]

# Calculating the valuefunction values for each type/year combination -----------------

setwd("C:\\Users\\Andreas\\Documents\\GitHub\\DynamicProgramming_TermPaper\\Term paper model")

source("functions.R")


initial.parameters <- c(-9.5, 0.02, 0.1, -0.2, 0.006)

#  Get the estimated value functions by the FOC of MLE problem

value.tilde <- array(rep(NaN, n.types * (n.neighborhoods + 1) * t), c(n.types,(n.neighborhoods + 1), n.periods))

for (m in 1:n.types){
  print(m)
  for (t in 1:n.periods){
    
    value.tilde[m, , t] <- log(shares.moving[m, , t]) - mean(log(shares.moving[m, ,t]))
    
  }
}

log(0)

log(shares.moving[m, , t]) - mean(log(shares.moving[m, ,t]))

mean(log(shares.moving[2,,1]))

log.sharea = log(shares.moving)

log.sharea[,,]

ml.share = mean(log.sharea[1,,1])
