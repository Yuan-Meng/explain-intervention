# Clears workspace:  
rm (list = ls(all = TRUE)) 
# Sets working directories:
setwd("") # Insert your working directory 
# Load packages:
library(R2jags)

# Part 1: Load data ------------------
# All data
everyone <- read.csv("/explain-intervention/data/anonymized.csv", header = T) 
everyone$X <- NULL
# Subsetting 
## Children
children <- everyone[which(everyone$group=='children'),] # child data
children_base <- children[which(children$condition=='baseline'),]
children_exp <- children[which(children$condition=='explanation'),]
children_rep <- children[which(children$condition=='report'),]

## Adults
adults <- everyone[which(everyone$group=='adults'),] # adult data  
adults_base <- adults[which(adults$condition=='baseline'),]
adults_exp <- adults[which(adults$condition=='explanation'),]
adults_rep <- adults[which(adults$condition=='report'),]

# Part 2: Model fitting  ------------------
puzzle.name <- paste("p", 1:6, sep="") 
m <- 6 # of puzzles for each participant 
l <- 3 # of nodes in each puzzle

# Children: Baseline
## Basic info
node <- children_base[puzzle.name] # Intervention choices 
n <- nrow(node) # of participants 

## Evaluate nodes
### According to IG
IG <- c(c(0.0, 1.0, 0.0), c(0.0, 0.0, 1.0), c(1.0, 0.0, 0.0), c(1.0, 0.0, 0.0), c(0.0, 1.0, 0.0), c(0.0, 0.0, 1.0))
IG <- matrix(IG, nrow=6, ncol=3, byrow=T)
V_IG <- array(0, dim=c(n, m, l)) 
for (i in 1:n){
  for (j in 1:m){
    for (k in 1:l)
    {
      V_IG[i,j,k] <- IG[j,k] 
    }
  }
} 

### According to PTS
PTS <- c(c(1.0, 0.5, 0.0), c(1.0, 0.0, 0.5), c(1.0, 1.0, 0.0), c(1.0, 0.0, 1.0), c(0.0, 0.5, 1.0), c(0.0, 1.0, 0.5))
PTS <- matrix(PTS, nrow=m, ncol=3, byrow=T)
V_PTS <- array(0, dim=c(n, m, l)) 
for (i in 1:n){
  for (j in 1:m){
    for (k in 1:l)
    {
      V_PTS[i,j,k] <- PTS[j,k] 
    }
  }
} 

### By random assignment 
random <- rep(c(1,1,1), 6)
random <- matrix(random, nrow=m, ncol=3, byrow=T)
V_rand <- array(0, dim=c(n, m, l)) 
for (i in 1:n){
  for (j in 1:m){
    for (k in 1:l)
    {
      V_rand[i,j,k] <- random[j,k]
    }
  }
} 

## Initialization 
### Single models
myinits_single <- list(
  list(tau = rep(1,n)), 
  list(tau = rep(5,n)), 
  list(tau = rep(10,n)), 
  list(tau = rep(15,n)), 
  list(tau = rep(20,n)), 
  list(tau = rep(25,n)), 
  list(tau = rep(30,n)), 
  list(tau = rep(35,n)), 
  list(tau = rep(40,n)), 
  list(tau = rep(45,n)), 
  list(tau = rep(50,n)), 
  list(tau = rep(55,n)), 
  list(tau = rep(60,n)), 
  list(tau = rep(65,n)), 
  list(tau = rep(70,n)), 
  list(tau = rep(75,n)), 
  list(tau = rep(80,n)), 
  list(tau = rep(85,n)), 
  list(tau = rep(90,n)), 
  list(tau = rep(95,n)), 
  list(tau = rep(100,n)),
  list(tau = rep(105,n)),
  list(tau = rep(110,n)),
  list(tau = rep(115,n)),
  list(tau = rep(120,n)),
  list(tau = rep(125,n)),
  list(tau = rep(130,n)),
  list(tau = rep(135,n)),
  list(tau = rep(140,n)),
  list(tau = rep(145,n))
) 
parameters_single <- c("tau")

### Combined models
myinits_combined <- list(list(tau = rep(1,n), theta = rep(0.9,n)),
                         list(tau = rep(5,n), theta = rep(0.1,n)),
                         list(tau = rep(10,n), theta = rep(0.8,n)),
                         list(tau = rep(15,n), theta = rep(0.2,n)),
                         list(tau = rep(20,n), theta = rep(0.7,n)),
                         list(tau = rep(25,n), theta = rep(0.3,n)),
                         list(tau = rep(30,n), theta = rep(0.6,n)),
                         list(tau = rep(35,n), theta = rep(0.4,n)),
                         list(tau = rep(40,n), theta = rep(0.5,n)),
                         list(tau = rep(45,n), theta = rep(0.5,n)),
                         list(tau = rep(50,n), theta = rep(0.1,n)),
                         list(tau = rep(55,n), theta = rep(0.9,n)),
                         list(tau = rep(60,n), theta = rep(0.2,n)),
                         list(tau = rep(65,n), theta = rep(0.8,n)),
                         list(tau = rep(70,n), theta = rep(0.3,n)),
                         list(tau = rep(75,n), theta = rep(0.7,n)),
                         list(tau = rep(80,n), theta = rep(0.4,n)),
                         list(tau = rep(85,n), theta = rep(0.6,n)),
                         list(tau = rep(90,n), theta = rep(0.5,n)),
                         list(tau = rep(95,n), theta = rep(0.5,n)),
                         list(tau = rep(100,n), theta = rep(0.5,n)),
                         list(tau = rep(105,n), theta = rep(0.5,n)),
                         list(tau = rep(110,n), theta = rep(0.1,n)),
                         list(tau = rep(115,n), theta = rep(0.9,n)),
                         list(tau = rep(120,n), theta = rep(0.2,n)),
                         list(tau = rep(125,n), theta = rep(0.8,n)),
                         list(tau = rep(130,n), theta = rep(0.3,n)),
                         list(tau = rep(135,n), theta = rep(0.7,n)),
                         list(tau = rep(140,n), theta = rep(0.4,n)),
                         list(tau = rep(145,n), theta = rep(0.6,n))
) 
parameters_combined <- c("tau", "theta", "mu")

## Data 
data_IG <- list("n", "m", "V_IG", "node") 
data_PTS <- list("n", "m", "V_PTS", "node")  
data_rand <- list("n", "m", "V_rand", "node") 
data_EIG_PTS <- list("n", "m", "V_IG", "V_PTS", "node") 
data_PTS_rand <- list("n", "m", "V_PTS", "V_rand", "node")

## Models 
### IG
set.seed(123)
IG_children_base <- jags(data=data_IG, inits=myinits_single, parameters=parameters_single,
                             model.file ="IG.txt", n.chains=30, n.iter=100000, 
                             n.burnin=1000, n.thin=10, DIC=T)
save(IG_children_base, file="IG_children_base.rda")
### PTS
set.seed(123)
PTS_children_base <- jags(data=data_PTS, inits=myinits_single, parameters=parameters_single,
                         model.file ="PTS.txt", n.chains=30, n.iter=100000, 
                         n.burnin=1000, n.thin=10, DIC=T)

save(PTS_children_base, file="PTS_children_base.rda")
### Random
set.seed(123)
rand_children_base <- jags(data=data_rand, inits=myinits_single, parameters=parameters_single,
                          model.file ="random.txt", n.chains=30, n.iter=100000, 
                          n.burnin=1000, n.thin=10, DIC=T)

save(rand_children_base, file="rand_children_base.rda")
### EIG + PTS
set.seed(123)
EIG_PTS_children_base <- jags(data=data_EIG_PTS, inits=myinits_combined, parameters=parameters_combined,
                           model.file ="EIG+PTS.txt", n.chains=30, n.iter=100000, 
                           n.burnin=1000, n.thin=10, DIC=T)

save(EIG_PTS_children_base, file="EIG_PTS_children_base.rda")
children_base_mu <- EIG_PTS_children_base$BUGSoutput$sims.list$mu
save(children_base_mu, file = "children_base_mu.Rdata")

### PTS + random
set.seed(123)
PTS_rand_children_base <- jags(data=data_PTS_rand, inits=myinits_combined, parameters=parameters_combined,
                              model.file ="PTS+rand.txt", n.chains=30, n.iter=100000, 
                              n.burnin=1000, n.thin=10, DIC=T)

save(PTS_rand_children_base, file="PTS_rand_children_base.rda")
children_base_PTS_mu <- PTS_rand_children_base$BUGSoutput$sims.list$mu
save(children_base_PTS_mu, file = "children_base_PTS_mu")

# Children: Explanation 
## Basic info
node <- children_exp[puzzle.name] # Intervention choices 
n <- nrow(node) # of participants 

## Evaluate nodes
### According to IG
IG <- c(c(0.0, 1.0, 0.0), c(0.0, 0.0, 1.0), c(1.0, 0.0, 0.0), c(1.0, 0.0, 0.0), c(0.0, 1.0, 0.0), c(0.0, 0.0, 1.0))
IG <- matrix(IG, nrow=6, ncol=3, byrow=T)
V_IG <- array(0, dim=c(n, m, l)) 
for (i in 1:n){
  for (j in 1:m){
    for (k in 1:l)
    {
      V_IG[i,j,k] <- IG[j,k] 
    }
  }
} 

### According to PTS
PTS <- c(c(1.0, 0.5, 0.0), c(1.0, 0.0, 0.5), c(1.0, 1.0, 0.0), c(1.0, 0.0, 1.0), c(0.0, 0.5, 1.0), c(0.0, 1.0, 0.5))
PTS <- matrix(PTS, nrow=m, ncol=3, byrow=T)
V_PTS <- array(0, dim=c(n, m, l)) 
for (i in 1:n){
  for (j in 1:m){
    for (k in 1:l)
    {
      V_PTS[i,j,k] <- PTS[j,k] 
    }
  }
} 

### By random assignment 
random <- rep(c(1,1,1), 6)
random <- matrix(random, nrow=m, ncol=3, byrow=T)
V_rand <- array(0, dim=c(n, m, l)) 
for (i in 1:n){
  for (j in 1:m){
    for (k in 1:l)
    {
      V_rand[i,j,k] <- random[j,k]
    }
  }
} 

## Initialization 
### Single models
myinits_single <- list(
  list(tau = rep(1,n)), 
  list(tau = rep(5,n)), 
  list(tau = rep(10,n)), 
  list(tau = rep(15,n)), 
  list(tau = rep(20,n)), 
  list(tau = rep(25,n)), 
  list(tau = rep(30,n)), 
  list(tau = rep(35,n)), 
  list(tau = rep(40,n)), 
  list(tau = rep(45,n)), 
  list(tau = rep(50,n)), 
  list(tau = rep(55,n)), 
  list(tau = rep(60,n)), 
  list(tau = rep(65,n)), 
  list(tau = rep(70,n)), 
  list(tau = rep(75,n)), 
  list(tau = rep(80,n)), 
  list(tau = rep(85,n)), 
  list(tau = rep(90,n)), 
  list(tau = rep(95,n)), 
  list(tau = rep(100,n)),
  list(tau = rep(105,n)),
  list(tau = rep(110,n)),
  list(tau = rep(115,n)),
  list(tau = rep(120,n)),
  list(tau = rep(125,n)),
  list(tau = rep(130,n)),
  list(tau = rep(135,n)),
  list(tau = rep(140,n)),
  list(tau = rep(145,n))
) 
parameters_single <- c("tau")

### Combined models
myinits_combined <- list(list(tau = rep(1,n), theta = rep(0.9,n)),
                         list(tau = rep(5,n), theta = rep(0.1,n)),
                         list(tau = rep(10,n), theta = rep(0.8,n)),
                         list(tau = rep(15,n), theta = rep(0.2,n)),
                         list(tau = rep(20,n), theta = rep(0.7,n)),
                         list(tau = rep(25,n), theta = rep(0.3,n)),
                         list(tau = rep(30,n), theta = rep(0.6,n)),
                         list(tau = rep(35,n), theta = rep(0.4,n)),
                         list(tau = rep(40,n), theta = rep(0.5,n)),
                         list(tau = rep(45,n), theta = rep(0.5,n)),
                         list(tau = rep(50,n), theta = rep(0.1,n)),
                         list(tau = rep(55,n), theta = rep(0.9,n)),
                         list(tau = rep(60,n), theta = rep(0.2,n)),
                         list(tau = rep(65,n), theta = rep(0.8,n)),
                         list(tau = rep(70,n), theta = rep(0.3,n)),
                         list(tau = rep(75,n), theta = rep(0.7,n)),
                         list(tau = rep(80,n), theta = rep(0.4,n)),
                         list(tau = rep(85,n), theta = rep(0.6,n)),
                         list(tau = rep(90,n), theta = rep(0.5,n)),
                         list(tau = rep(95,n), theta = rep(0.5,n)),
                         list(tau = rep(100,n), theta = rep(0.5,n)),
                         list(tau = rep(105,n), theta = rep(0.5,n)),
                         list(tau = rep(110,n), theta = rep(0.1,n)),
                         list(tau = rep(115,n), theta = rep(0.9,n)),
                         list(tau = rep(120,n), theta = rep(0.2,n)),
                         list(tau = rep(125,n), theta = rep(0.8,n)),
                         list(tau = rep(130,n), theta = rep(0.3,n)),
                         list(tau = rep(135,n), theta = rep(0.7,n)),
                         list(tau = rep(140,n), theta = rep(0.4,n)),
                         list(tau = rep(145,n), theta = rep(0.6,n))
) 
parameters_combined <- c("tau", "theta", "mu")

## Data 
data_IG <- list("n", "m", "V_IG", "node") 
data_PTS <- list("n", "m", "V_PTS", "node")  
data_rand <- list("n", "m", "V_rand", "node") 
data_EIG_PTS <- list("n", "m", "V_IG", "V_PTS", "node") 
data_PTS_rand <- list("n", "m", "V_PTS", "V_rand", "node")

## Models 
### IG
set.seed(123)
IG_children_exp <- jags(data=data_IG, inits=myinits_single, parameters=parameters_single,
                         model.file ="IG.txt", n.chains=30, n.iter=100000, 
                         n.burnin=1000, n.thin=10, DIC=T)

save(IG_children_exp, file="IG_children_exp.rda")
### PTS
set.seed(123)
PTS_children_exp <- jags(data=data_PTS, inits=myinits_single, parameters=parameters_single,
                          model.file ="PTS.txt", n.chains=30, n.iter=100000, 
                          n.burnin=1000, n.thin=10, DIC=T)

save(PTS_children_exp, file="PTS_children_exp.rda")
### Random
set.seed(123)
rand_children_exp <- jags(data=data_rand, inits=myinits_single, parameters=parameters_single,
                           model.file ="random.txt", n.chains=30, n.iter=100000, 
                           n.burnin=1000, n.thin=10, DIC=T)

save(rand_children_exp, file="rand_children_exp.rda")
### EIG + PTS
set.seed(123)
EIG_PTS_children_exp <- jags(data=data_EIG_PTS, inits=myinits_combined, parameters=parameters_combined,
                              model.file ="EIG+PTS.txt", n.chains=30, n.iter=100000, 
                              n.burnin=1000, n.thin=10, DIC=T)

save(EIG_PTS_children_exp, file="EIG_PTS_children_exp.rda")
children_exp_mu <- EIG_PTS_children_exp$BUGSoutput$sims.list$mu
save(children_exp_mu, file = "children_exp_mu.Rdata")

### PTS + random
set.seed(123)
PTS_rand_children_exp <- jags(data=data_PTS_rand, inits=myinits_combined, parameters=parameters_combined,
                               model.file ="PTS+rand.txt", n.chains=30, n.iter=100000, 
                               n.burnin=1000, n.thin=10, DIC=T)

save(PTS_rand_children_exp, file="PTS_rand_children_exp.rda")
children_exp_PTS_mu <- PTS_rand_children_exp$BUGSoutput$sims.list$mu
save(children_exp_PTS_mu, file = "children_exp_PTS_mu")

# Children: Report 
## Basic info
node <- children_rep[puzzle.name] # Intervention choices 
n <- nrow(node) # of participants 

## Evaluate nodes
### According to IG
IG <- c(c(0.0, 1.0, 0.0), c(0.0, 0.0, 1.0), c(1.0, 0.0, 0.0), c(1.0, 0.0, 0.0), c(0.0, 1.0, 0.0), c(0.0, 0.0, 1.0))
IG <- matrix(IG, nrow=6, ncol=3, byrow=T)
V_IG <- array(0, dim=c(n, m, l)) 
for (i in 1:n){
  for (j in 1:m){
    for (k in 1:l)
    {
      V_IG[i,j,k] <- IG[j,k] 
    }
  }
} 

### According to PTS
PTS <- c(c(1.0, 0.5, 0.0), c(1.0, 0.0, 0.5), c(1.0, 1.0, 0.0), c(1.0, 0.0, 1.0), c(0.0, 0.5, 1.0), c(0.0, 1.0, 0.5))
PTS <- matrix(PTS, nrow=m, ncol=3, byrow=T)
V_PTS <- array(0, dim=c(n, m, l)) 
for (i in 1:n){
  for (j in 1:m){
    for (k in 1:l)
    {
      V_PTS[i,j,k] <- PTS[j,k] 
    }
  }
} 

### By random assignment 
random <- rep(c(1,1,1), 6)
random <- matrix(random, nrow=m, ncol=3, byrow=T)
V_rand <- array(0, dim=c(n, m, l)) 
for (i in 1:n){
  for (j in 1:m){
    for (k in 1:l)
    {
      V_rand[i,j,k] <- random[j,k]
    }
  }
} 

## Initialization 
### Single models
myinits_single <- list(
  list(tau = rep(1,n)), 
  list(tau = rep(5,n)), 
  list(tau = rep(10,n)), 
  list(tau = rep(15,n)), 
  list(tau = rep(20,n)), 
  list(tau = rep(25,n)), 
  list(tau = rep(30,n)), 
  list(tau = rep(35,n)), 
  list(tau = rep(40,n)), 
  list(tau = rep(45,n)), 
  list(tau = rep(50,n)), 
  list(tau = rep(55,n)), 
  list(tau = rep(60,n)), 
  list(tau = rep(65,n)), 
  list(tau = rep(70,n)), 
  list(tau = rep(75,n)), 
  list(tau = rep(80,n)), 
  list(tau = rep(85,n)), 
  list(tau = rep(90,n)), 
  list(tau = rep(95,n)), 
  list(tau = rep(100,n)),
  list(tau = rep(105,n)),
  list(tau = rep(110,n)),
  list(tau = rep(115,n)),
  list(tau = rep(120,n)),
  list(tau = rep(125,n)),
  list(tau = rep(130,n)),
  list(tau = rep(135,n)),
  list(tau = rep(140,n)),
  list(tau = rep(145,n))
) 
parameters_single <- c("tau")

### Combined models
myinits_combined <- list(list(tau = rep(1,n), theta = rep(0.9,n)),
                         list(tau = rep(5,n), theta = rep(0.1,n)),
                         list(tau = rep(10,n), theta = rep(0.8,n)),
                         list(tau = rep(15,n), theta = rep(0.2,n)),
                         list(tau = rep(20,n), theta = rep(0.7,n)),
                         list(tau = rep(25,n), theta = rep(0.3,n)),
                         list(tau = rep(30,n), theta = rep(0.6,n)),
                         list(tau = rep(35,n), theta = rep(0.4,n)),
                         list(tau = rep(40,n), theta = rep(0.5,n)),
                         list(tau = rep(45,n), theta = rep(0.5,n)),
                         list(tau = rep(50,n), theta = rep(0.1,n)),
                         list(tau = rep(55,n), theta = rep(0.9,n)),
                         list(tau = rep(60,n), theta = rep(0.2,n)),
                         list(tau = rep(65,n), theta = rep(0.8,n)),
                         list(tau = rep(70,n), theta = rep(0.3,n)),
                         list(tau = rep(75,n), theta = rep(0.7,n)),
                         list(tau = rep(80,n), theta = rep(0.4,n)),
                         list(tau = rep(85,n), theta = rep(0.6,n)),
                         list(tau = rep(90,n), theta = rep(0.5,n)),
                         list(tau = rep(95,n), theta = rep(0.5,n)),
                         list(tau = rep(100,n), theta = rep(0.5,n)),
                         list(tau = rep(105,n), theta = rep(0.5,n)),
                         list(tau = rep(110,n), theta = rep(0.1,n)),
                         list(tau = rep(115,n), theta = rep(0.9,n)),
                         list(tau = rep(120,n), theta = rep(0.2,n)),
                         list(tau = rep(125,n), theta = rep(0.8,n)),
                         list(tau = rep(130,n), theta = rep(0.3,n)),
                         list(tau = rep(135,n), theta = rep(0.7,n)),
                         list(tau = rep(140,n), theta = rep(0.4,n)),
                         list(tau = rep(145,n), theta = rep(0.6,n))
) 
parameters_combined <- c("tau", "theta", "mu")

## Data 
data_IG <- list("n", "m", "V_IG", "node") 
data_PTS <- list("n", "m", "V_PTS", "node")  
data_rand <- list("n", "m", "V_rand", "node") 
data_EIG_PTS <- list("n", "m", "V_IG", "V_PTS", "node") 
data_PTS_rand <- list("n", "m", "V_PTS", "V_rand", "node")

## Models 
### IG
set.seed(123)
IG_children_rep <- jags(data=data_IG, inits=myinits_single, parameters=parameters_single,
                        model.file ="IG.txt", n.chains=30, n.iter=100000, 
                        n.burnin=1000, n.thin=10, DIC=T)

save(IG_children_rep, file="IG_children_rep.rda")
### PTS
set.seed(123)
PTS_children_rep <- jags(data=data_PTS, inits=myinits_single, parameters=parameters_single,
                         model.file ="PTS.txt", n.chains=30, n.iter=100000, 
                         n.burnin=1000, n.thin=10, DIC=T)

save(PTS_children_rep, file="PTS_children_rep.rda")
### Random
set.seed(123)
rand_children_rep <- jags(data=data_rand, inits=myinits_single, parameters=parameters_single,
                          model.file ="random.txt", n.chains=30, n.iter=100000, 
                          n.burnin=1000, n.thin=10, DIC=T)

save(rand_children_rep, file="rand_children_rep.rda")
### EIG + PTS
set.seed(123)
EIG_PTS_children_rep <- jags(data=data_EIG_PTS, inits=myinits_combined, parameters=parameters_combined,
                             model.file ="EIG+PTS.txt", n.chains=30, n.iter=100000, 
                             n.burnin=1000, n.thin=10, DIC=T)

save(EIG_PTS_children_rep, file="EIG_PTS_children_rep.rda")
children_rep_mu <- EIG_PTS_children_rep$BUGSoutput$sims.list$mu
save(children_rep_mu, file = "children_rep_mu.Rdata")

### PTS + random
set.seed(123)
PTS_rand_children_rep <- jags(data=data_PTS_rand, inits=myinits_combined, parameters=parameters_combined,
                              model.file ="PTS+rand.txt", n.chains=30, n.iter=100000, 
                              n.burnin=1000, n.thin=10, DIC=T)

save(PTS_rand_children_rep, file="PTS_rand_children_rep.rda")
children_rep_PTS_mu <- PTS_rand_children_rep$BUGSoutput$sims.list$mu
save(children_rep_PTS_mu, file = "children_rep_PTS_mu.Rdata")

# Adults: Baseline
## Basic info
node <- adults_base[puzzle.name] # Intervention choices 
n <- nrow(node) # of participants 

## Evaluate nodes
### According to IG
IG <- c(c(0.0, 1.0, 0.0), c(0.0, 0.0, 1.0), c(1.0, 0.0, 0.0), c(1.0, 0.0, 0.0), c(0.0, 1.0, 0.0), c(0.0, 0.0, 1.0))
IG <- matrix(IG, nrow=6, ncol=3, byrow=T)
V_IG <- array(0, dim=c(n, m, l)) 
for (i in 1:n){
  for (j in 1:m){
    for (k in 1:l)
    {
      V_IG[i,j,k] <- IG[j,k] 
    }
  }
} 

### According to PTS
PTS <- c(c(1.0, 0.5, 0.0), c(1.0, 0.0, 0.5), c(1.0, 1.0, 0.0), c(1.0, 0.0, 1.0), c(0.0, 0.5, 1.0), c(0.0, 1.0, 0.5))
PTS <- matrix(PTS, nrow=m, ncol=3, byrow=T)
V_PTS <- array(0, dim=c(n, m, l)) 
for (i in 1:n){
  for (j in 1:m){
    for (k in 1:l)
    {
      V_PTS[i,j,k] <- PTS[j,k] 
    }
  }
} 

### By random assignment 
random <- rep(c(1,1,1), 6)
random <- matrix(random, nrow=m, ncol=3, byrow=T)
V_rand <- array(0, dim=c(n, m, l)) 
for (i in 1:n){
  for (j in 1:m){
    for (k in 1:l)
    {
      V_rand[i,j,k] <- random[j,k]
    }
  }
} 

## Initialization 
### Single models
myinits_single <- list(
  list(tau = rep(1,n)), 
  list(tau = rep(5,n)), 
  list(tau = rep(10,n)), 
  list(tau = rep(15,n)), 
  list(tau = rep(20,n)), 
  list(tau = rep(25,n)), 
  list(tau = rep(30,n)), 
  list(tau = rep(35,n)), 
  list(tau = rep(40,n)), 
  list(tau = rep(45,n)), 
  list(tau = rep(50,n)), 
  list(tau = rep(55,n)), 
  list(tau = rep(60,n)), 
  list(tau = rep(65,n)), 
  list(tau = rep(70,n)), 
  list(tau = rep(75,n)), 
  list(tau = rep(80,n)), 
  list(tau = rep(85,n)), 
  list(tau = rep(90,n)), 
  list(tau = rep(95,n)), 
  list(tau = rep(100,n)),
  list(tau = rep(105,n)),
  list(tau = rep(110,n)),
  list(tau = rep(115,n)),
  list(tau = rep(120,n)),
  list(tau = rep(125,n)),
  list(tau = rep(130,n)),
  list(tau = rep(135,n)),
  list(tau = rep(140,n)),
  list(tau = rep(145,n))
) 
parameters_single <- c("tau")

### Combined models
myinits_combined <- list(list(tau = rep(1,n), theta = rep(0.9,n)),
                         list(tau = rep(5,n), theta = rep(0.1,n)),
                         list(tau = rep(10,n), theta = rep(0.8,n)),
                         list(tau = rep(15,n), theta = rep(0.2,n)),
                         list(tau = rep(20,n), theta = rep(0.7,n)),
                         list(tau = rep(25,n), theta = rep(0.3,n)),
                         list(tau = rep(30,n), theta = rep(0.6,n)),
                         list(tau = rep(35,n), theta = rep(0.4,n)),
                         list(tau = rep(40,n), theta = rep(0.5,n)),
                         list(tau = rep(45,n), theta = rep(0.5,n)),
                         list(tau = rep(50,n), theta = rep(0.1,n)),
                         list(tau = rep(55,n), theta = rep(0.9,n)),
                         list(tau = rep(60,n), theta = rep(0.2,n)),
                         list(tau = rep(65,n), theta = rep(0.8,n)),
                         list(tau = rep(70,n), theta = rep(0.3,n)),
                         list(tau = rep(75,n), theta = rep(0.7,n)),
                         list(tau = rep(80,n), theta = rep(0.4,n)),
                         list(tau = rep(85,n), theta = rep(0.6,n)),
                         list(tau = rep(90,n), theta = rep(0.5,n)),
                         list(tau = rep(95,n), theta = rep(0.5,n)),
                         list(tau = rep(100,n), theta = rep(0.5,n)),
                         list(tau = rep(105,n), theta = rep(0.5,n)),
                         list(tau = rep(110,n), theta = rep(0.1,n)),
                         list(tau = rep(115,n), theta = rep(0.9,n)),
                         list(tau = rep(120,n), theta = rep(0.2,n)),
                         list(tau = rep(125,n), theta = rep(0.8,n)),
                         list(tau = rep(130,n), theta = rep(0.3,n)),
                         list(tau = rep(135,n), theta = rep(0.7,n)),
                         list(tau = rep(140,n), theta = rep(0.4,n)),
                         list(tau = rep(145,n), theta = rep(0.6,n))
) 
parameters_combined <- c("tau", "theta", "mu")

## Data 
data_IG <- list("n", "m", "V_IG", "node") 
data_PTS <- list("n", "m", "V_PTS", "node")  
data_rand <- list("n", "m", "V_rand", "node") 
data_EIG_PTS <- list("n", "m", "V_IG", "V_PTS", "node") 
data_PTS_rand <- list("n", "m", "V_PTS", "V_rand", "node")

## Models 
### IG
set.seed(123)
IG_adults_base <- jags(data=data_IG, inits=myinits_single, parameters=parameters_single,
                       model.file ="IG.txt", n.chains=30, n.iter=100000, 
                       n.burnin=1000, n.thin=10, DIC=T)
save(IG_adults_base, file="IG_adults_base.rda")
### PTS
set.seed(123)
PTS_adults_base <- jags(data=data_PTS, inits=myinits_single, parameters=parameters_single,
                        model.file ="PTS.txt", n.chains=30, n.iter=100000, 
                        n.burnin=1000, n.thin=10, DIC=T)

save(PTS_adults_base, file="PTS_adults_base.rda")
### Random
set.seed(123)
rand_adults_base <- jags(data=data_rand, inits=myinits_single, parameters=parameters_single,
                         model.file ="random.txt", n.chains=30, n.iter=100000, 
                         n.burnin=1000, n.thin=10, DIC=T)

save(rand_adults_base, file="rand_adults_base.rda")
### EIG + PTS
set.seed(123)
EIG_PTS_adults_base <- jags(data=data_EIG_PTS, inits=myinits_combined, parameters=parameters_combined,
                            model.file ="EIG+PTS.txt", n.chains=30, n.iter=100000, 
                            n.burnin=1000, n.thin=10, DIC=T)

save(EIG_PTS_adults_base, file="EIG_PTS_adults_base.rda")
adults_base_mu <- EIG_PTS_adults_base$BUGSoutput$sims.list$mu
save(adults_base_mu, file = "adults_base_mu.Rdata")

### PTS + random
set.seed(123)
PTS_rand_adults_base <- jags(data=data_PTS_rand, inits=myinits_combined, parameters=parameters_combined,
                             model.file ="PTS+rand.txt", n.chains=30, n.iter=100000, 
                             n.burnin=1000, n.thin=10, DIC=T)

save(PTS_rand_adults_base, file="PTS_rand_adults_base.rda")
adults_base_PTS_mu <- PTS_rand_adults_base$BUGSoutput$sims.list$mu
save(adults_base_PTS_mu, file = "adults_base_PTS_mu.Rdata")

# Adults: Explanation 
## Basic info
node <- adults_exp[puzzle.name] # Intervention choices 
n <- nrow(node) # of participants 

## Evaluate nodes
### According to IG
IG <- c(c(0.0, 1.0, 0.0), c(0.0, 0.0, 1.0), c(1.0, 0.0, 0.0), c(1.0, 0.0, 0.0), c(0.0, 1.0, 0.0), c(0.0, 0.0, 1.0))
IG <- matrix(IG, nrow=6, ncol=3, byrow=T)
V_IG <- array(0, dim=c(n, m, l)) 
for (i in 1:n){
  for (j in 1:m){
    for (k in 1:l)
    {
      V_IG[i,j,k] <- IG[j,k] 
    }
  }
} 

### According to PTS
PTS <- c(c(1.0, 0.5, 0.0), c(1.0, 0.0, 0.5), c(1.0, 1.0, 0.0), c(1.0, 0.0, 1.0), c(0.0, 0.5, 1.0), c(0.0, 1.0, 0.5))
PTS <- matrix(PTS, nrow=m, ncol=3, byrow=T)
V_PTS <- array(0, dim=c(n, m, l)) 
for (i in 1:n){
  for (j in 1:m){
    for (k in 1:l)
    {
      V_PTS[i,j,k] <- PTS[j,k] 
    }
  }
} 

### By random assignment 
random <- rep(c(1,1,1), 6)
random <- matrix(random, nrow=m, ncol=3, byrow=T)
V_rand <- array(0, dim=c(n, m, l)) 
for (i in 1:n){
  for (j in 1:m){
    for (k in 1:l)
    {
      V_rand[i,j,k] <- random[j,k]
    }
  }
} 

## Initialization 
### Single models
myinits_single <- list(
  list(tau = rep(1,n)), 
  list(tau = rep(5,n)), 
  list(tau = rep(10,n)), 
  list(tau = rep(15,n)), 
  list(tau = rep(20,n)), 
  list(tau = rep(25,n)), 
  list(tau = rep(30,n)), 
  list(tau = rep(35,n)), 
  list(tau = rep(40,n)), 
  list(tau = rep(45,n)), 
  list(tau = rep(50,n)), 
  list(tau = rep(55,n)), 
  list(tau = rep(60,n)), 
  list(tau = rep(65,n)), 
  list(tau = rep(70,n)), 
  list(tau = rep(75,n)), 
  list(tau = rep(80,n)), 
  list(tau = rep(85,n)), 
  list(tau = rep(90,n)), 
  list(tau = rep(95,n)), 
  list(tau = rep(100,n)),
  list(tau = rep(105,n)),
  list(tau = rep(110,n)),
  list(tau = rep(115,n)),
  list(tau = rep(120,n)),
  list(tau = rep(125,n)),
  list(tau = rep(130,n)),
  list(tau = rep(135,n)),
  list(tau = rep(140,n)),
  list(tau = rep(145,n))
) 
parameters_single <- c("tau")

### Combined models
myinits_combined <- list(list(tau = rep(1,n), theta = rep(0.9,n)),
                         list(tau = rep(5,n), theta = rep(0.1,n)),
                         list(tau = rep(10,n), theta = rep(0.8,n)),
                         list(tau = rep(15,n), theta = rep(0.2,n)),
                         list(tau = rep(20,n), theta = rep(0.7,n)),
                         list(tau = rep(25,n), theta = rep(0.3,n)),
                         list(tau = rep(30,n), theta = rep(0.6,n)),
                         list(tau = rep(35,n), theta = rep(0.4,n)),
                         list(tau = rep(40,n), theta = rep(0.5,n)),
                         list(tau = rep(45,n), theta = rep(0.5,n)),
                         list(tau = rep(50,n), theta = rep(0.1,n)),
                         list(tau = rep(55,n), theta = rep(0.9,n)),
                         list(tau = rep(60,n), theta = rep(0.2,n)),
                         list(tau = rep(65,n), theta = rep(0.8,n)),
                         list(tau = rep(70,n), theta = rep(0.3,n)),
                         list(tau = rep(75,n), theta = rep(0.7,n)),
                         list(tau = rep(80,n), theta = rep(0.4,n)),
                         list(tau = rep(85,n), theta = rep(0.6,n)),
                         list(tau = rep(90,n), theta = rep(0.5,n)),
                         list(tau = rep(95,n), theta = rep(0.5,n)),
                         list(tau = rep(100,n), theta = rep(0.5,n)),
                         list(tau = rep(105,n), theta = rep(0.5,n)),
                         list(tau = rep(110,n), theta = rep(0.1,n)),
                         list(tau = rep(115,n), theta = rep(0.9,n)),
                         list(tau = rep(120,n), theta = rep(0.2,n)),
                         list(tau = rep(125,n), theta = rep(0.8,n)),
                         list(tau = rep(130,n), theta = rep(0.3,n)),
                         list(tau = rep(135,n), theta = rep(0.7,n)),
                         list(tau = rep(140,n), theta = rep(0.4,n)),
                         list(tau = rep(145,n), theta = rep(0.6,n))
) 
parameters_combined <- c("tau", "theta", "mu")

## Data 
data_IG <- list("n", "m", "V_IG", "node") 
data_PTS <- list("n", "m", "V_PTS", "node")  
data_rand <- list("n", "m", "V_rand", "node") 
data_EIG_PTS <- list("n", "m", "V_IG", "V_PTS", "node") 
data_PTS_rand <- list("n", "m", "V_PTS", "V_rand", "node")

## Models 
### IG
set.seed(123)
IG_adults_exp <- jags(data=data_IG, inits=myinits_single, parameters=parameters_single,
                      model.file ="IG.txt", n.chains=30, n.iter=100000, 
                      n.burnin=1000, n.thin=10, DIC=T)

save(IG_adults_exp, file="IG_adults_exp.rda")
### PTS
set.seed(123)
PTS_adults_exp <- jags(data=data_PTS, inits=myinits_single, parameters=parameters_single,
                       model.file ="PTS.txt", n.chains=30, n.iter=100000, 
                       n.burnin=1000, n.thin=10, DIC=T)

save(PTS_adults_exp, file="PTS_adults_exp.rda")
### Random
set.seed(123)
rand_adults_exp <- jags(data=data_rand, inits=myinits_single, parameters=parameters_single,
                        model.file ="random.txt", n.chains=30, n.iter=100000, 
                        n.burnin=1000, n.thin=10, DIC=T)

save(rand_adults_exp, file="rand_adults_exp.rda")
### EIG + PTS
set.seed(123)
EIG_PTS_adults_exp <- jags(data=data_EIG_PTS, inits=myinits_combined, parameters=parameters_combined,
                           model.file ="EIG+PTS.txt", n.chains=30, n.iter=100000, 
                           n.burnin=1000, n.thin=10, DIC=T)

save(EIG_PTS_adults_exp, file="EIG_PTS_adults_exp.rda")
adults_exp_mu <- EIG_PTS_adults_exp$BUGSoutput$sims.list$mu
save(adults_exp_mu, file = "adults_exp_mu.Rdata")

### PTS + random
set.seed(123)
PTS_rand_adults_exp <- jags(data=data_PTS_rand, inits=myinits_combined, parameters=parameters_combined,
                            model.file ="PTS+rand.txt", n.chains=30, n.iter=100000, 
                            n.burnin=1000, n.thin=10, DIC=T)

save(PTS_rand_adults_exp, file="PTS_rand_adults_exp.rda")
adults_exp_PTS_mu <- PTS_rand_adults_exp$BUGSoutput$sims.list$mu
save(adults_exp_PTS_mu, file = "adults_exp_PTS_mu.Rdata")

# Adults: Report 
## Basic info
node <- adults_rep[puzzle.name] # Intervention choices 
n <- nrow(node) # of participants 

## Evaluate nodes
### According to IG
IG <- c(c(0.0, 1.0, 0.0), c(0.0, 0.0, 1.0), c(1.0, 0.0, 0.0), c(1.0, 0.0, 0.0), c(0.0, 1.0, 0.0), c(0.0, 0.0, 1.0))
IG <- matrix(IG, nrow=6, ncol=3, byrow=T)
V_IG <- array(0, dim=c(n, m, l)) 
for (i in 1:n){
  for (j in 1:m){
    for (k in 1:l)
    {
      V_IG[i,j,k] <- IG[j,k] 
    }
  }
} 

### According to PTS
PTS <- c(c(1.0, 0.5, 0.0), c(1.0, 0.0, 0.5), c(1.0, 1.0, 0.0), c(1.0, 0.0, 1.0), c(0.0, 0.5, 1.0), c(0.0, 1.0, 0.5))
PTS <- matrix(PTS, nrow=m, ncol=3, byrow=T)
V_PTS <- array(0, dim=c(n, m, l)) 
for (i in 1:n){
  for (j in 1:m){
    for (k in 1:l)
    {
      V_PTS[i,j,k] <- PTS[j,k] 
    }
  }
} 

### By random assignment 
random <- rep(c(1,1,1), 6)
random <- matrix(random, nrow=m, ncol=3, byrow=T)
V_rand <- array(0, dim=c(n, m, l)) 
for (i in 1:n){
  for (j in 1:m){
    for (k in 1:l)
    {
      V_rand[i,j,k] <- random[j,k]
    }
  }
} 

## Initialization 
### Single models
myinits_single <- list(
  list(tau = rep(1,n)), 
  list(tau = rep(5,n)), 
  list(tau = rep(10,n)), 
  list(tau = rep(15,n)), 
  list(tau = rep(20,n)), 
  list(tau = rep(25,n)), 
  list(tau = rep(30,n)), 
  list(tau = rep(35,n)), 
  list(tau = rep(40,n)), 
  list(tau = rep(45,n)), 
  list(tau = rep(50,n)), 
  list(tau = rep(55,n)), 
  list(tau = rep(60,n)), 
  list(tau = rep(65,n)), 
  list(tau = rep(70,n)), 
  list(tau = rep(75,n)), 
  list(tau = rep(80,n)), 
  list(tau = rep(85,n)), 
  list(tau = rep(90,n)), 
  list(tau = rep(95,n)), 
  list(tau = rep(100,n)),
  list(tau = rep(105,n)),
  list(tau = rep(110,n)),
  list(tau = rep(115,n)),
  list(tau = rep(120,n)),
  list(tau = rep(125,n)),
  list(tau = rep(130,n)),
  list(tau = rep(135,n)),
  list(tau = rep(140,n)),
  list(tau = rep(145,n))
) 
parameters_single <- c("tau")

### Combined models
myinits_combined <- list(list(tau = rep(1,n), theta = rep(0.9,n)),
                         list(tau = rep(5,n), theta = rep(0.1,n)),
                         list(tau = rep(10,n), theta = rep(0.8,n)),
                         list(tau = rep(15,n), theta = rep(0.2,n)),
                         list(tau = rep(20,n), theta = rep(0.7,n)),
                         list(tau = rep(25,n), theta = rep(0.3,n)),
                         list(tau = rep(30,n), theta = rep(0.6,n)),
                         list(tau = rep(35,n), theta = rep(0.4,n)),
                         list(tau = rep(40,n), theta = rep(0.5,n)),
                         list(tau = rep(45,n), theta = rep(0.5,n)),
                         list(tau = rep(50,n), theta = rep(0.1,n)),
                         list(tau = rep(55,n), theta = rep(0.9,n)),
                         list(tau = rep(60,n), theta = rep(0.2,n)),
                         list(tau = rep(65,n), theta = rep(0.8,n)),
                         list(tau = rep(70,n), theta = rep(0.3,n)),
                         list(tau = rep(75,n), theta = rep(0.7,n)),
                         list(tau = rep(80,n), theta = rep(0.4,n)),
                         list(tau = rep(85,n), theta = rep(0.6,n)),
                         list(tau = rep(90,n), theta = rep(0.5,n)),
                         list(tau = rep(95,n), theta = rep(0.5,n)),
                         list(tau = rep(100,n), theta = rep(0.5,n)),
                         list(tau = rep(105,n), theta = rep(0.5,n)),
                         list(tau = rep(110,n), theta = rep(0.1,n)),
                         list(tau = rep(115,n), theta = rep(0.9,n)),
                         list(tau = rep(120,n), theta = rep(0.2,n)),
                         list(tau = rep(125,n), theta = rep(0.8,n)),
                         list(tau = rep(130,n), theta = rep(0.3,n)),
                         list(tau = rep(135,n), theta = rep(0.7,n)),
                         list(tau = rep(140,n), theta = rep(0.4,n)),
                         list(tau = rep(145,n), theta = rep(0.6,n))
) 
parameters_combined <- c("tau", "theta", "mu")

## Data 
data_IG <- list("n", "m", "V_IG", "node") 
data_PTS <- list("n", "m", "V_PTS", "node")  
data_rand <- list("n", "m", "V_rand", "node") 
data_EIG_PTS <- list("n", "m", "V_IG", "V_PTS", "node") 
data_PTS_rand <- list("n", "m", "V_PTS", "V_rand", "node")

## Models 
### IG
set.seed(123)
IG_adults_rep <- jags(data=data_IG, inits=myinits_single, parameters=parameters_single,
                      model.file ="IG.txt", n.chains=30, n.iter=100000, 
                      n.burnin=1000, n.thin=10, DIC=T)

save(IG_adults_rep, file="IG_adults_rep.rda")
### PTS
set.seed(123)
PTS_adults_rep <- jags(data=data_PTS, inits=myinits_single, parameters=parameters_single,
                       model.file ="PTS.txt", n.chains=30, n.iter=100000, 
                       n.burnin=1000, n.thin=10, DIC=T)

save(PTS_adults_rep, file="PTS_adults_rep.rda")
### Random
set.seed(123)
rand_adults_rep <- jags(data=data_rand, inits=myinits_single, parameters=parameters_single,
                        model.file ="random.txt", n.chains=30, n.iter=100000, 
                        n.burnin=1000, n.thin=10, DIC=T)

save(rand_adults_rep, file="rand_adults_rep.rda")
### EIG + PTS
set.seed(123)
EIG_PTS_adults_rep <- jags(data=data_EIG_PTS, inits=myinits_combined, parameters=parameters_combined,
                           model.file ="EIG+PTS.txt", n.chains=30, n.iter=100000, 
                           n.burnin=1000, n.thin=10, DIC=T)

save(EIG_PTS_adults_rep, file="EIG_PTS_adults_rep.rda")
adults_rep_mu <- EIG_PTS_adults_rep$BUGSoutput$sims.list$mu
save(adults_rep_mu, file = "adults_rep_mu.Rdata")

### PTS + random
set.seed(123)
PTS_rand_adults_rep <- jags(data=data_PTS_rand, inits=myinits_combined, parameters=parameters_combined,
                            model.file ="PTS+rand.txt", n.chains=30, n.iter=100000, 
                            n.burnin=1000, n.thin=10, DIC=T)

save(PTS_rand_adults_rep, file="PTS_rand_adults_rep.rda")
adults_rep_PTS_mu <- PTS_rand_adults_rep$BUGSoutput$sims.list$mu
save(adults_rep_PTS_mu, file = "adults_rep_PTS_mu.Rdata")

# Part 3: Learning effects  ------------------
puzzle.name <- paste("p", 1:6, sep="") 
m <- 6 # of puzzles for each participant 
l <- 3 # of nodes in each puzzle
