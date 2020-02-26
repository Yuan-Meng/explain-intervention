# Clears workspace:  
rm (list = ls(all = TRUE)) 
# Sets working directories:
setwd("/Users/apple/Desktop/My folders/Research/projects/inProgress/EI/data/combined/analysis")
# Load packages:
library(R2jags)

# Part 1: Create BUGs scripts ------------------
## IG
modelString = "
model{
  # Observed interventions 
  for (i in 1:n){ # For each participant
    for (j in 1:m){ # For each puzzle
      node[i,j] ~ dcat(prob[i,j,1:3]) # Which node to choose
      prob[i,j,1:3] <- exp_v[i,j,1:3] / sum(exp_v[i,j,1:3]) # Probability of choosing each node 
      for (k in 1:3){ # The value of each node
        exp_v[i,j,k] <- exp(V_IG[i,j,k]/tau[i])
        }
    }
  }
  # Prior(s) on parameter(s)
  for (i in 1:n){
    tau[i] ~ dgamma(sh, ra)
  }
  # Reparameterize shape and rate by mean and standard deviation
  sh <- pow(mean,2) / pow(sd,2) # shape
  ra <- mean / pow(sd,2) # rate
  mean ~ dunif(0,100) # mean 
  sd ~ dunif(0,100) # standard deviation 
}
"
writeLines(modelString, con="IG.txt")

## PTS
modelString = "
model{
  # Observed interventions 
  for (i in 1:n){ # For each participant
    for (j in 1:m){ # For each puzzle
      node[i,j] ~ dcat(prob[i,j,1:3]) # Which node to choose
      prob[i,j,1:3] <- exp_v[i,j,1:3] / sum(exp_v[i,j,1:3]) # Probability of choosing each node 
      for (k in 1:3){ # The value of each node
        exp_v[i,j,k] <- exp(V_PTS[i,j,k]/tau[i])
      }
    }
  }
  # Prior(s) on parameter(s)
  for (i in 1:n){
    tau[i] ~ dgamma(sh, ra)
  }
  # Reparameterize shape and rate by mean and standard deviation
  sh <- pow(mean,2) / pow(sd,2) # shape
  ra <- mean / pow(sd,2) # rate
  mean ~ dunif(0,100) # mean 
  sd ~ dunif(0,100) # standard deviation 
}
"
writeLines(modelString, con="PTS.txt")

## Random
modelString = "
model{
  # Observed interventions 
  for (i in 1:n){ # For each participant
    for (j in 1:m){ # For each puzzle
      node[i,j] ~ dcat(prob[i,j,1:3]) # Which node to choose
      prob[i,j,1:3] <- exp_v[i,j,1:3] / sum(exp_v[i,j,1:3]) # Probability of choosing each node 
      for (k in 1:3){ # The value of each node
        exp_v[i,j,k] <- exp(V_rand[i,j,k]/tau[i])
      }
    }
  }
  # Prior(s) on parameter(s)
  for (i in 1:n){
    tau[i] ~ dgamma(sh, ra)
  }
  # Reparameterize shape and rate by mean and standard deviation
  sh <- pow(mean,2) / pow(sd,2) # shape
  ra <- mean / pow(sd,2) # rate
  mean ~ dunif(0,100) # mean 
  sd ~ dunif(0,100) # standard deviation 
}
"
writeLines(modelString, con="random.txt")

## EIG + PTS
modelString = "
model{
  # Observed interventions 
  for(i in 1:n){ # For each participant
    for (j in 1:m){ # For each puzzle
      node[i,j] ~ dcat(prob[i,j,1:3]) # Which node to choose
      prob[i,j,1:3] <- exp_v[i,j,1:3] / sum(exp_v[i,j,1:3]) # Probability of choosing each node 
      for (k in 1:3) {
        exp_v[i,j,k] <- exp((V_IG[i,j,k]*theta[i] + V_PTS[i,j,k]*(1-theta[i]))/tau[i])
      }
    }
  }
  # Prior(s) on parameter(s)
  for (i in 1:n) {
    tau[i] ~ dgamma(sh, ra)
    theta[i] ~ dbeta(alpha, beta) T(0,0.999)
  }
  # Reparameterize shape and rate by mean and standard deviation
  sh <- pow(mean,2) / pow(sd,2)
  ra <- mean / pow(sd,2)
  mean ~ dunif(0,100)
  sd ~ dunif(0,100)
  # Reparameterize alpha and beta by mean and variation
  alpha <- mu * phi
  beta <- (1-mu) * phi
  phi ~ dgamma(0.001,0.001)
  mu ~ dbeta(0.5,0.5) 
}
"
writeLines(modelString, con="EIG+PTS.txt")

## PTS + random 
modelString = "
model{
  # Observed interventions 
  for(i in 1:n){ # For each participant 
    for (j in 1:m){ # For each puzzle
    node[i,j] ~ dcat(prob[i,j,1:3]) # Which node to choose
    prob[i,j,1:3] <- exp_v[i,j,1:3] / sum(exp_v[i,j,1:3]) # Probability of choosing each node 
    for (k in 1:3) {
      exp_v[i,j,k] <- exp((V_PTS[i,j,k]*theta[i] + V_rand[i,j,k]*(1-theta[i]))/tau[i])
      }
    }
  }
  # Prior(s) on parameter(s)
  for (i in 1:n) {
    tau[i] ~ dgamma(sh, ra)
    theta[i] ~ dbeta(alpha, beta) T(0,0.999)
  }
  # Reparameterize shape and rate by mean and standard deviation
  sh <- pow(mean,2) / pow(sd,2)
  ra <- mean / pow(sd,2)
  mean ~ dunif(0,100)
  sd ~ dunif(0,100)
  # Reparameterize alpha and beta by mean and variation
  alpha <- mu * phi
  beta <- (1-mu) * phi
  phi ~ dgamma(0.001,0.001)
  mu ~ dbeta(0.5,0.5) 
}
"
writeLines(modelString, con="PTS+rand.txt")