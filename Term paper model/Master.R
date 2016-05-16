
# Masters

setwd("C:\\Users\\Langholz\\Documents\\GitHub\\DynamicProgramming_TermPaper\\Term paper model")

library(tidyr)
library(dplyr)
library(MASS)
library(dummies)

rm(list = ls())

# Parameters
beta <- 0.95
Euler <- 0.577215665

# Load functions used in estimations
source("functions.R")

# Prepare material for step 1 & 2 - splitting into types and Calculating CCP's 
source("Stage_12_dataPrep.R")

# Estimation of step 1 & 2 - Deriving gamma parameter values from maximum likelihood
source("Estimation of step 1 and 2.R")

# Preparation of step 3 
source()



# Estimation of step 3 - Decomposing per period utility
source()