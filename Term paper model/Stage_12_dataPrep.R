

library(tidyr)
library(dplyr)

zdata = read.csv("C:\\Users\\Andreas\\Documents\\GitHub\\DynamicProgramming_TermPaper\\simuleret data.csv", header = TRUE, sep = ",")

# parameters & Initial values -----------------------------

T <- length(unique(zdata$current.year))

neighboorhood <- unique(zdata$area)

nr.nhood <- length(neighboorhood)

nr.obs <- nrow(zdata)

moving.costs <- 0.06  # omkostningerne ved at sælge ( undersøg om det er de samme i DK)

years <- unique(zdata$current.year)

# indicators for years and neighborhoods

for (i in 1:nr.obs) {
 zdata$year.ind[i] = which(years == zdata$current.year[i])
}

for(i in 1:nr.obs) {
  zdata$nhood[i] = which(neighboorhood == zdata$area[i])
}

# Splitting observations in types based on income and wealth ----------------------
nr.incometypes = 5
nr.wealthtypes = 5

income.max = max(zdata$income)
income.min = min(zdata$income)
income.bins = seq(income.min, income.max, income.max / nr.incometypes)
income.bins[nr.incometypes + 1] <- Inf                                # Det maksimale loft for indtægt i den sidste gruppe er uendeligt

wealth.max = max(zdata$wealth)
wealth.min = min(zdata$wealth)
wealth.bins = seq(wealth.min, wealth.max, wealth.max / nr.wealthtypes)
wealth.bins[nr.wealthtypes +1 ] = Inf                                 # det maksimale loft for formue i det sidste led er uendeligt

nr.types <- nr.wealthtypes * nr.incometypes

type.matrix <- as.data.frame(matrix(1:nr.types,nr.wealthtypes,nr.incometypes))

for (i in 1:nr.obs) {
  for (j in 1:nr.incometypes) {
    for (w in 1:nr.wealthtypes) {
      
      if(zdata$income[i] >= income.bins[j] & zdata$income[i] < income.bins[j + 1]
       & zdata$wealth[i] >= wealth.bins[w] & zdata$wealth[i] < wealth.bins[w + 1]) zdata$type.tau[i] <- type.matrix[w,j]
        
  }
 } 
}


# New number of types, as we cant loop over types without content ## OBS Tjek lige om det her step er korrekt

nr.types <- length(unique(zdata$type.tau))

# Creating the Conditional Choice Probabilities by splittingon time, neighborhoods and years -------------------

# Creating frequency tables for observations of each type/year/hood combination and moving decisions

# nr in each combination
group.obs <- tbl_df(zdata) %>%
  group_by(type.tau, year.ind, nhood) %>% 
  summarise(nr = n())

fact.types <- c("type.tau","year.ind","nhood", "nr")
group.obs <- as.data.frame(lapply(group.obs[fact.types], as.factor))
full.grid = expand.grid(type.tau = levels(as.factor(zdata$type.tau)), year.ind = levels(as.factor(zdata$year.ind)), nhood = levels(as.factor(zdata$nhood)))
group.obs <- left_join(full.grid,group.obs)
group.obs$nr <- as.numeric(group.obs$nr)
group.obs$nr[is.na(group.obs$nr)] <- 0

# nr of each combination whith movement
group.obs.move <- tbl_df(zdata) %>%
  group_by(type.tau, year.ind, nhood, flyt) %>% 
  summarise(nr = n())

fact.types <- c("type.tau","year.ind","nhood","flyt", "nr")
group.obs.move <- as.data.frame(lapply(group.obs.move[fact.types], as.factor))
full.grid = expand.grid(type.tau = levels(as.factor(zdata$type.tau)), year.ind = levels(as.factor(zdata$year.ind)), nhood = levels(as.factor(zdata$nhood)), flyt = levels(as.factor(zdata$flyt)))
group.obs.move <- left_join(full.grid,group.obs.move)
group.obs.move$nr <- as.numeric(group.obs.move$nr)
group.obs.move$nr[is.na(group.obs.move$nr)] <- 0

# Calculating the % share of move/stay for each type/year/hood combination (man kunne overveje et ekstra if statement for at komme af NaN)

shares.moving <- array(0,dim=c(T,nr.types,nr.nhood + 1))

for (t in 1:T) {
  for (tau in 1:nr.types) {
     for (k in 1:nr.nhood) {
      
       sum.obs <- group.obs$nr[(group.obs$nhood == k) & (group.obs$type.tau == tau) & (group.obs$year.ind == t)] 
       sum.moving <- group.obs.move$nr[(group.obs.move$flyt == 1) & (group.obs.move$nhood == k) & (group.obs.move$type.tau == tau) & (group.obs.move$year.ind == t)] 
      
       if (length(sum.moving / sum.obs) == 0) {
         shares.moving[t,tau,k] = 0 
       }
       else if (sum.obs == 0 & sum.obs.moving > 0){
         shares.moving[t,tau,k] = 1
       }
       else {
         shares.moving[t,tau,k] = sum.moving / sum.obs
       }
    }
  }
}

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
for (t in 1:T) {
  stig.vec[t] <- (1+0.017)^t 
}

for (i in 1:nr.obs) {
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




















