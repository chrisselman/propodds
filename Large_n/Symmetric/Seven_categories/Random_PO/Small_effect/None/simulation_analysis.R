## Set working directory 
setwd("/group/cebu1/BACKUP/Chris/Project_2/Main_Simulations/Large_n/Symmetric/Seven_categories/Random_PO/Small_effect/None")


## Main simulation scenario 

start <- Sys.time()
rm(list=ls())

options(mc.cores = 8)
## Load in the appropriate packages
library("rstan")
library('posterior')
library("rmsb")

args = commandArgs(trailingOnly = TRUE)
datnum <- as.numeric(args[1])

# Load the 1000 datasets
load("largen_sym_7cat_randompo_smalleff_none_dataset.Rdata")
attach(big_data)

# Select a subset of the data
s2 = seq(20,1000,by=20)
s1=s2-19

temp = big_data[(big_data$k >= s1[datnum] & big_data$k <= s2[datnum]),]

# Set seed
set.seed(071023)
seed=sample(1:1e8,50,replace=F)[datnum]

set.seed(seed)
nsim <- 3
states <- c("A", "B", "C","D","E","F","G")

#Generate empty vectors to store performance measures and diagnostic measures 
## Store bias for each cumulative logit for each analysis model  
for (i in 1:(length(states) - 1)){
  assign(paste0("bias_cumlogit_po",i),length(nsim))
  assign(paste0("bias_cumlogit_ppo",i),length(nsim))
  assign(paste0("bias_cumlogit_cpo",i),length(nsim))
  assign(paste0("bias_cumlogit_lcpo",i),length(nsim))
  assign(paste0("bias_cumlogit_lr",i),length(nsim))
}

## Store coverage for each cumulative logit for each analysis model  
for (i in 1:(length(states) - 1)){
  assign(paste0("cv_cumlogit_po",i),length(nsim))
  assign(paste0("cv_cumlogit_ppo",i),length(nsim))
  assign(paste0("cv_cumlogit_cpo",i),length(nsim))
  assign(paste0("cv_cumlogit_lcpo",i),length(nsim))
  assign(paste0("cv_cumlogit_lr",i),length(nsim))
}


## Store MSE for each cumulative logit for each analysis model  
for (i in 1:(length(states) - 1)){
  assign(paste0("mse_cumlogit_po",i),length(nsim))
  assign(paste0("mse_cumlogit_ppo",i),length(nsim))
  assign(paste0("mse_cumlogit_cpo",i),length(nsim))
  assign(paste0("mse_cumlogit_lcpo",i),length(nsim))
  assign(paste0("mse_cumlogit_lr",i),length(nsim))
}


## Store R-hat for each parameter of each cumulative logit for each analysis model  
for (i in 1:(length(states) - 1)){
  assign(paste0("rhat_cumlogit_po",i),length(nsim))
  assign(paste0("rhat_cumlogit_ppo",i),length(nsim))
  assign(paste0("rhat_cumlogit_cpo",i),length(nsim))
  assign(paste0("rhat_cumlogit_lcpo",i),length(nsim))
  assign(paste0("rhat_cumlogit_lr",i),length(nsim))
}


## Store bulk ESS for each parameter of each cumulative logit for each analysis model  
for (i in 1:(length(states) - 1)){
  assign(paste0("bulkess_cumlogit_po",i),length(nsim))
  assign(paste0("bulkess_cumlogit_ppo",i),length(nsim))
  assign(paste0("bulkess_cumlogit_cpo",i),length(nsim))
  assign(paste0("bulkess_cumlogit_lcpo",i),length(nsim))
  assign(paste0("bulkess_cumlogit_lr",i),length(nsim))
}

## Store tail ESS for each parameter of each cumulative logit for each analysis model  
for (i in 1:(length(states) - 1)){
  assign(paste0("tailess_cumlogit_po",i),length(nsim))
  assign(paste0("tailess_cumlogit_ppo",i),length(nsim))
  assign(paste0("tailess_cumlogit_cpo",i),length(nsim))
  assign(paste0("tailess_cumlogit_lcpo",i),length(nsim))
  assign(paste0("tailess_cumlogit_lr",i),length(nsim))
}

## Store MCSE for each parameter of each cumulative logit for each analysis model  
for (i in 1:(length(states) - 1)){
  assign(paste0("mcse_cumlogit_po",i),length(nsim))
  assign(paste0("mcse_cumlogit_ppo",i),length(nsim))
  assign(paste0("mcse_cumlogit_cpo",i),length(nsim))
  assign(paste0("mcse_cumlogit_lcpo",i),length(nsim))
  assign(paste0("mcse_cumlogit_lr",i),length(nsim))
}


## Missing values 
missing_po <- missing_ppo <- missing_cpo <- missing_lcpo <- missing_lr1 <- missing_lr2 <- missing_lr3 <- missing_lr4 <- missing_lr5 <- missing_lr6 <- length(nsim) # to store number of missing values in each model 


## Number of divergent transitions 
numdivergent_po <- numdivergent_ppo <- numdivergent_cpo <- numdivergent_lcpo <- numdivergent_lr1 <- numdivergent_lr2 <- numdivergent_lr3 <- numdivergent_lr4 <- numdivergent_lr5 <- numdivergent_lr6 <- length(nsim) # to store number of divergent transitions in each model 


# To specify prior SDs for contrasts using blrm for PO and logistic models 
. <- function(...) list(...)

for (k in s1[datnum]:s2[datnum]){
  x <- big_data$x[big_data$k == k]
  y <- big_data$y[big_data$k == k]
  x1 <- big_data$x1[big_data$k == k]
  y1 <- big_data$y1[big_data$k == k]
  y2 <- big_data$y2[big_data$k == k]
  y3 <- big_data$y3[big_data$k == k]
  y4 <- big_data$y4[big_data$k == k]
  y5 <- big_data$y5[big_data$k == k]
  y6 <- big_data$y6[big_data$k == k]
  
  
  ##################### Proportional odds model ##################
  pomod <- blrm(y~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                  contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=7500,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(pomod$draws)
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_cumlogit_po1[k] <- as.numeric(summarise_draws(df, "median")[7,2]) - log(1.10)
  bias_cumlogit_po2[k] <- as.numeric(summarise_draws(df, "median")[7,2]) - log(1.10)
  bias_cumlogit_po3[k] <- as.numeric(summarise_draws(df, "median")[7,2]) - log(1.10)
  bias_cumlogit_po4[k] <- as.numeric(summarise_draws(df, "median")[7,2]) - log(1.10)
  bias_cumlogit_po5[k] <- as.numeric(summarise_draws(df, "median")[7,2]) - log(1.10)
  bias_cumlogit_po6[k] <- as.numeric(summarise_draws(df, "median")[7,2]) - log(1.10)

  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[7,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[7,3])

  cv_cumlogit_po1[k] <- ifelse(lci < log(1.10) & uci > log(1.10), 1, 0)
  cv_cumlogit_po2[k] <- ifelse(lci < log(1.10) & uci > log(1.10), 1, 0)
  cv_cumlogit_po3[k] <- ifelse(lci < log(1.10) & uci > log(1.10), 1, 0)
  cv_cumlogit_po4[k] <- ifelse(lci < log(1.10) & uci > log(1.10), 1, 0)
  cv_cumlogit_po5[k] <- ifelse(lci < log(1.10) & uci > log(1.10), 1, 0)
  cv_cumlogit_po6[k] <- ifelse(lci < log(1.10) & uci > log(1.10), 1, 0)

  
  # MSE 
  mse_cumlogit_po1[k] <- (as.numeric(summarise_draws(df, "median")[7,2]) - log(1.10))^2
  mse_cumlogit_po2[k] <- (as.numeric(summarise_draws(df, "median")[7,2]) - log(1.10))^2
  mse_cumlogit_po3[k] <- (as.numeric(summarise_draws(df, "median")[7,2]) - log(1.10))^2
  mse_cumlogit_po4[k] <- (as.numeric(summarise_draws(df, "median")[7,2]) - log(1.10))^2
  mse_cumlogit_po5[k] <- (as.numeric(summarise_draws(df, "median")[7,2]) - log(1.10))^2
  mse_cumlogit_po6[k] <- (as.numeric(summarise_draws(df, "median")[7,2]) - log(1.10))^2
  
  # MCSE 
  mcse_cumlogit_po1[k] <- as.numeric(summarise_draws(df, "mcse_median")[7,2])
  mcse_cumlogit_po2[k] <- as.numeric(summarise_draws(df, "mcse_median")[7,2])
  mcse_cumlogit_po3[k] <- as.numeric(summarise_draws(df, "mcse_median")[7,2])
  mcse_cumlogit_po4[k] <- as.numeric(summarise_draws(df, "mcse_median")[7,2])
  mcse_cumlogit_po5[k] <- as.numeric(summarise_draws(df, "mcse_median")[7,2])
  mcse_cumlogit_po6[k] <- as.numeric(summarise_draws(df, "mcse_median")[7,2])

  
  # R-hat 
  rhat_cumlogit_po1[k] <- as.numeric(summarise_draws(df, "rhat")[7,2])
  rhat_cumlogit_po2[k] <- as.numeric(summarise_draws(df, "rhat")[7,2])
  rhat_cumlogit_po3[k] <- as.numeric(summarise_draws(df, "rhat")[7,2])
  rhat_cumlogit_po4[k] <- as.numeric(summarise_draws(df, "rhat")[7,2])
  rhat_cumlogit_po5[k] <- as.numeric(summarise_draws(df, "rhat")[7,2])
  rhat_cumlogit_po6[k] <- as.numeric(summarise_draws(df, "rhat")[7,2])

  
  # Bulk ESS 
  bulkess_cumlogit_po1[k] <- as.numeric(summarise_draws(df, "ess_bulk")[7,2])
  bulkess_cumlogit_po2[k] <- as.numeric(summarise_draws(df, "ess_bulk")[7,2])
  bulkess_cumlogit_po3[k] <- as.numeric(summarise_draws(df, "ess_bulk")[7,2])
  bulkess_cumlogit_po4[k] <- as.numeric(summarise_draws(df, "ess_bulk")[7,2])
  bulkess_cumlogit_po5[k] <- as.numeric(summarise_draws(df, "ess_bulk")[7,2])
  bulkess_cumlogit_po6[k] <- as.numeric(summarise_draws(df, "ess_bulk")[7,2])

  
  # Tail ESS 
  tailess_cumlogit_po1[k] <- as.numeric(summarise_draws(df, "ess_tail")[7,2])
  tailess_cumlogit_po2[k] <- as.numeric(summarise_draws(df, "ess_tail")[7,2])
  tailess_cumlogit_po3[k] <- as.numeric(summarise_draws(df, "ess_tail")[7,2])
  tailess_cumlogit_po4[k] <- as.numeric(summarise_draws(df, "ess_tail")[7,2])
  tailess_cumlogit_po5[k] <- as.numeric(summarise_draws(df, "ess_tail")[7,2])
  tailess_cumlogit_po6[k] <- as.numeric(summarise_draws(df, "ess_tail")[7,2])

 
  
  # Number of divergent transitions 
  numdivergent_po[k] <- get_num_divergent(stanGet(pomod))
  
  
  # Number of missing values
  missing_po[k] <- length(pomod$na.action)
  
  
  ############### Unconstrained partial proportional odds model ###################
  ppomod <- blrm(y~x,ppo = ~x, priorsdppo = 100,
                 conc = 1, seed=1234,iter=7500,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(ppomod$draws)
  
  phi1 <- mutate_variables(df, phi = df$x + df$`x:y>=3`)
  phi1 <- subset_draws(phi1, c("phi"))
  
  phi2 <- mutate_variables(df, phi = df$x + df$`x:y>=4`)
  phi2 <- subset_draws(phi2, c("phi"))
  
  phi3 <- mutate_variables(df, phi = df$x + df$`x:y>=5`)
  phi3 <- subset_draws(phi3, c("phi"))
  
  phi4 <- mutate_variables(df, phi = df$x + df$`x:y>=6`)
  phi4 <- subset_draws(phi4, c("phi"))
  
  phi5 <- mutate_variables(df, phi = df$x + df$`x:y>=7`)
  phi5 <- subset_draws(phi5, c("phi"))
  
  

  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_cumlogit_ppo1[k] <- as.numeric(summarise_draws(df, "median")[7,2]) - log(1.10)
  bias_cumlogit_ppo2[k] <- as.numeric(summarise_draws(phi1, "median")[1,2]) - log(1.10)
  bias_cumlogit_ppo3[k] <- as.numeric(summarise_draws(phi2, "median")[1,2]) - log(1.10)
  bias_cumlogit_ppo4[k] <- as.numeric(summarise_draws(phi3, "median")[1,2]) - log(1.10)
  bias_cumlogit_ppo5[k] <- as.numeric(summarise_draws(phi4, "median")[1,2]) - log(1.10)
  bias_cumlogit_ppo6[k] <- as.numeric(summarise_draws(phi5, "median")[1,2]) - log(1.10)

  
  # Coverage - does the true value fall in the credible interval?
  lci1 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[7,2])
  uci1 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[7,3])
  
  lci2 <- as.numeric(summarise_draws(phi1, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci2 <- as.numeric(summarise_draws(phi1, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  lci3 <- as.numeric(summarise_draws(phi2, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci3 <- as.numeric(summarise_draws(phi2, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  lci4 <- as.numeric(summarise_draws(phi3, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci4 <- as.numeric(summarise_draws(phi3, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  lci5 <- as.numeric(summarise_draws(phi4, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci5 <- as.numeric(summarise_draws(phi4, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  lci6 <- as.numeric(summarise_draws(phi5, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci6 <- as.numeric(summarise_draws(phi5, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  

  
  cv_cumlogit_ppo1[k] <- ifelse(lci1 < log(1.10) & uci1 > log(1.10), 1, 0)
  cv_cumlogit_ppo2[k] <- ifelse(lci2 < log(1.10) & uci2 > log(1.10), 1, 0)
  cv_cumlogit_ppo3[k] <- ifelse(lci3 < log(1.10) & uci3 > log(1.10), 1, 0)
  cv_cumlogit_ppo4[k] <- ifelse(lci4 < log(1.10) & uci4 > log(1.10), 1, 0)
  cv_cumlogit_ppo5[k] <- ifelse(lci5 < log(1.10) & uci5 > log(1.10), 1, 0)
  cv_cumlogit_ppo6[k] <- ifelse(lci6 < log(1.10) & uci6 > log(1.10), 1, 0)

  
  # MSE 
  mse_cumlogit_ppo1[k] <- (as.numeric(summarise_draws(df, "median")[7,2]) - log(1.10))^2
  mse_cumlogit_ppo2[k] <- (as.numeric(summarise_draws(phi1, "median")[1,2]) - log(1.10))^2
  mse_cumlogit_ppo3[k] <- (as.numeric(summarise_draws(phi2, "median")[1,2]) - log(1.10))^2
  mse_cumlogit_ppo4[k] <- (as.numeric(summarise_draws(phi3, "median")[1,2]) - log(1.10))^2
  mse_cumlogit_ppo5[k] <- (as.numeric(summarise_draws(phi4, "median")[1,2]) - log(1.10))^2
  mse_cumlogit_ppo6[k] <- (as.numeric(summarise_draws(phi5, "median")[1,2]) - log(1.10))^2

  
  # MCSE 
  mcse_cumlogit_ppo1[k] <- as.numeric(summarise_draws(df, "mcse_median")[7,2])
  mcse_cumlogit_ppo2[k] <- as.numeric(summarise_draws(phi1, "mcse_median")[1,2])
  mcse_cumlogit_ppo3[k] <- as.numeric(summarise_draws(phi2, "mcse_median")[1,2])
  mcse_cumlogit_ppo4[k] <- as.numeric(summarise_draws(phi3, "mcse_median")[1,2])
  mcse_cumlogit_ppo5[k] <- as.numeric(summarise_draws(phi4, "mcse_median")[1,2])
  mcse_cumlogit_ppo6[k] <- as.numeric(summarise_draws(phi5, "mcse_median")[1,2])

  
  # R-hat 
  rhat_cumlogit_ppo1[k] <- as.numeric(summarise_draws(df, "rhat")[7,2])
  rhat_cumlogit_ppo2[k] <- as.numeric(summarise_draws(phi1, "rhat")[1,2])
  rhat_cumlogit_ppo3[k] <- as.numeric(summarise_draws(phi2, "rhat")[1,2])
  rhat_cumlogit_ppo4[k] <- as.numeric(summarise_draws(phi3, "rhat")[1,2])
  rhat_cumlogit_ppo5[k] <- as.numeric(summarise_draws(phi4, "rhat")[1,2])
  rhat_cumlogit_ppo6[k] <- as.numeric(summarise_draws(phi5, "rhat")[1,2])

  
  # Bulk ESS 
  bulkess_cumlogit_ppo1[k] <- as.numeric(summarise_draws(df, "ess_bulk")[7,2])
  bulkess_cumlogit_ppo2[k] <- as.numeric(summarise_draws(phi1, "ess_bulk")[1,2])
  bulkess_cumlogit_ppo3[k] <- as.numeric(summarise_draws(phi2, "ess_bulk")[1,2])
  bulkess_cumlogit_ppo4[k] <- as.numeric(summarise_draws(phi3, "ess_bulk")[1,2])
  bulkess_cumlogit_ppo5[k] <- as.numeric(summarise_draws(phi4, "ess_bulk")[1,2])
  bulkess_cumlogit_ppo6[k] <- as.numeric(summarise_draws(phi5, "ess_bulk")[1,2])

  
  # Tail ESS 
  tailess_cumlogit_ppo1[k] <- as.numeric(summarise_draws(df, "ess_tail")[7,2])
  tailess_cumlogit_ppo2[k] <- as.numeric(summarise_draws(phi1, "ess_tail")[1,2])
  tailess_cumlogit_ppo3[k] <- as.numeric(summarise_draws(phi2, "ess_tail")[1,2])
  tailess_cumlogit_ppo4[k] <- as.numeric(summarise_draws(phi3, "ess_tail")[1,2])
  tailess_cumlogit_ppo5[k] <- as.numeric(summarise_draws(phi4, "ess_tail")[1,2])
  tailess_cumlogit_ppo6[k] <- as.numeric(summarise_draws(phi5, "ess_tail")[1,2])

  
  # Number of divergent transitions 
  numdivergent_ppo[k] <- get_num_divergent(stanGet(ppomod))
  
  
  # Number of missing values
  missing_ppo[k] <- length(ppomod$na.action)
  
  
  ################### CONSTRAINED PPO MODEL ASSUMING LINEARITY ################
  cppomodlin <- blrm(y~x,ppo = ~x, normcppo=T,
                     priorsdppo = 100,
                     conc = 1, seed=1234,iter=7500,chains=4,cppo=function(y) y,
                     sampling.args = list(cores = 4))

  h <- cppomodlin$cppo  
  
  df <- as_draws_df(cppomodlin$draws)
  
  phi1 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(2))
  phi1 <- subset_draws(phi1, c("phi"))
  
  phi2 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(3))
  phi2 <- subset_draws(phi2, c("phi"))
  
  phi3 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(4))
  phi3 <- subset_draws(phi3, c("phi"))
  
  phi4 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(5))
  phi4 <- subset_draws(phi4, c("phi"))
  
  phi5 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(6))
  phi5 <- subset_draws(phi5, c("phi"))
  
  phi6 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(7))
  phi6 <- subset_draws(phi6, c("phi"))


  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_cumlogit_lcpo1[k] <- as.numeric(summarise_draws(phi1, "median")[1,2]) - log(1.10)
  bias_cumlogit_lcpo2[k] <- as.numeric(summarise_draws(phi2, "median")[1,2]) - log(1.10)
  bias_cumlogit_lcpo3[k] <- as.numeric(summarise_draws(phi3, "median")[1,2]) - log(1.10)
  bias_cumlogit_lcpo4[k] <- as.numeric(summarise_draws(phi4, "median")[1,2]) - log(1.10)
  bias_cumlogit_lcpo5[k] <- as.numeric(summarise_draws(phi5, "median")[1,2]) - log(1.10)
  bias_cumlogit_lcpo6[k] <- as.numeric(summarise_draws(phi6, "median")[1,2]) - log(1.10)


  
  # Coverage - does the true value fall in the credible interval?
  lci1 <- as.numeric(summarise_draws(phi1, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci1 <- as.numeric(summarise_draws(phi1, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  lci2 <- as.numeric(summarise_draws(phi2, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci2 <- as.numeric(summarise_draws(phi2, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  lci3 <- as.numeric(summarise_draws(phi3, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci3 <- as.numeric(summarise_draws(phi3, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  lci4 <- as.numeric(summarise_draws(phi4, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci4 <- as.numeric(summarise_draws(phi4, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])

  lci5 <- as.numeric(summarise_draws(phi5, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci5 <- as.numeric(summarise_draws(phi5, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  lci6 <- as.numeric(summarise_draws(phi6, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci6 <- as.numeric(summarise_draws(phi6, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  
  
  cv_cumlogit_lcpo1[k] <- ifelse(lci1 < log(1.10) & uci1 > log(1.10), 1, 0)
  cv_cumlogit_lcpo2[k] <- ifelse(lci2 < log(1.10) & uci2 > log(1.10), 1, 0)
  cv_cumlogit_lcpo3[k] <- ifelse(lci3 < log(1.10) & uci3 > log(1.10), 1, 0)
  cv_cumlogit_lcpo4[k] <- ifelse(lci4 < log(1.10) & uci4 > log(1.10), 1, 0)
  cv_cumlogit_lcpo5[k] <- ifelse(lci5 < log(1.10) & uci5 > log(1.10), 1, 0)
  cv_cumlogit_lcpo6[k] <- ifelse(lci6 < log(1.10) & uci6 > log(1.10), 1, 0)
  
  # MSE 
  mse_cumlogit_lcpo1[k] <- (as.numeric(summarise_draws(phi1, "median")[1,2]) - log(1.10))^2
  mse_cumlogit_lcpo2[k] <- (as.numeric(summarise_draws(phi2, "median")[1,2]) - log(1.10))^2
  mse_cumlogit_lcpo3[k] <- (as.numeric(summarise_draws(phi3, "median")[1,2]) - log(1.10))^2
  mse_cumlogit_lcpo4[k] <- (as.numeric(summarise_draws(phi4, "median")[1,2]) - log(1.10))^2
  mse_cumlogit_lcpo5[k] <- (as.numeric(summarise_draws(phi5, "median")[1,2]) - log(1.10))^2
  mse_cumlogit_lcpo6[k] <- (as.numeric(summarise_draws(phi6, "median")[1,2]) - log(1.10))^2

  
  # MCSE 
  mcse_cumlogit_lcpo1[k] <- as.numeric(summarise_draws(phi1, "mcse_median")[1,2])
  mcse_cumlogit_lcpo2[k] <- as.numeric(summarise_draws(phi2, "mcse_median")[1,2])
  mcse_cumlogit_lcpo3[k] <- as.numeric(summarise_draws(phi3, "mcse_median")[1,2])
  mcse_cumlogit_lcpo4[k] <- as.numeric(summarise_draws(phi4, "mcse_median")[1,2])
  mcse_cumlogit_lcpo5[k] <- as.numeric(summarise_draws(phi5, "mcse_median")[1,2])
  mcse_cumlogit_lcpo6[k] <- as.numeric(summarise_draws(phi6, "mcse_median")[1,2])

  
  # R-hat 
  rhat_cumlogit_lcpo1[k] <- as.numeric(summarise_draws(phi1, "rhat")[1,2])
  rhat_cumlogit_lcpo2[k] <- as.numeric(summarise_draws(phi2, "rhat")[1,2])
  rhat_cumlogit_lcpo3[k] <- as.numeric(summarise_draws(phi3, "rhat")[1,2])
  rhat_cumlogit_lcpo4[k] <- as.numeric(summarise_draws(phi4, "rhat")[1,2])
  rhat_cumlogit_lcpo5[k] <- as.numeric(summarise_draws(phi5, "rhat")[1,2])
  rhat_cumlogit_lcpo6[k] <- as.numeric(summarise_draws(phi6, "rhat")[1,2])


  # Bulk ESS 
  bulkess_cumlogit_lcpo1[k] <- as.numeric(summarise_draws(phi1, "ess_bulk")[1,2])
  bulkess_cumlogit_lcpo2[k] <- as.numeric(summarise_draws(phi2, "ess_bulk")[1,2])
  bulkess_cumlogit_lcpo3[k] <- as.numeric(summarise_draws(phi3, "ess_bulk")[1,2])
  bulkess_cumlogit_lcpo4[k] <- as.numeric(summarise_draws(phi4, "ess_bulk")[1,2])
  bulkess_cumlogit_lcpo5[k] <- as.numeric(summarise_draws(phi5, "ess_bulk")[1,2])
  bulkess_cumlogit_lcpo6[k] <- as.numeric(summarise_draws(phi6, "ess_bulk")[1,2])


  
  # Tail ESS 
  tailess_cumlogit_lcpo1[k] <- as.numeric(summarise_draws(phi1, "ess_tail")[1,2])
  tailess_cumlogit_lcpo2[k] <- as.numeric(summarise_draws(phi2, "ess_tail")[1,2])
  tailess_cumlogit_lcpo3[k] <- as.numeric(summarise_draws(phi3, "ess_tail")[1,2])
  tailess_cumlogit_lcpo4[k] <- as.numeric(summarise_draws(phi4, "ess_tail")[1,2])
  tailess_cumlogit_lcpo5[k] <- as.numeric(summarise_draws(phi5, "ess_tail")[1,2])
  tailess_cumlogit_lcpo6[k] <- as.numeric(summarise_draws(phi6, "ess_tail")[1,2])

  
  # Number of divergent transitions 
  numdivergent_lcpo[k] <- get_num_divergent(stanGet(cppomodlin))
  
  
  # Number of missing values
  missing_lcpo[k] <- length(cppomodlin$na.action)
  
  
  ################### CONSTRAINED PPO MODEL ASSUMING DIVERGENCE ################
  
  cppomoddiv <- blrm(y~x,ppo = ~x, normcppo=F,
                     priorsdppo = 100,
                     conc = 1, seed=1234,iter=7500,chains=4,cppo=function(y) y==7,sampling.args = list(cores = 4))
  
  h <- cppomoddiv$cppo  
  
  df <- as_draws_df(cppomoddiv$draws)
  
  phi1 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(2))
  phi1 <- subset_draws(phi1, c("phi"))
  
  phi2 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(3))
  phi2 <- subset_draws(phi2, c("phi"))
  
  
  phi3 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(4))
  phi3 <- subset_draws(phi3, c("phi"))
  
  phi4 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(5))
  phi4 <- subset_draws(phi4, c("phi"))
  
  phi5 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(6))
  phi5 <- subset_draws(phi5, c("phi"))
  
  phi6 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(7))
  phi6 <- subset_draws(phi6, c("phi"))

  
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_cumlogit_cpo1[k] <- as.numeric(summarise_draws(phi1, "median")[1,2]) - log(1.10)
  bias_cumlogit_cpo2[k] <- as.numeric(summarise_draws(phi2, "median")[1,2]) - log(1.10)
  bias_cumlogit_cpo3[k] <- as.numeric(summarise_draws(phi3, "median")[1,2]) - log(1.10)
  bias_cumlogit_cpo4[k] <- as.numeric(summarise_draws(phi4, "median")[1,2]) - log(1.10)
  bias_cumlogit_cpo5[k] <- as.numeric(summarise_draws(phi5, "median")[1,2]) - log(1.10)
  bias_cumlogit_cpo6[k] <- as.numeric(summarise_draws(phi6, "median")[1,2]) - log(1.10)


  
  # Coverage - does the true value fall in the credible interval?
  lci1 <- as.numeric(summarise_draws(phi1, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci1 <- as.numeric(summarise_draws(phi1, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  lci2 <- as.numeric(summarise_draws(phi2, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci2 <- as.numeric(summarise_draws(phi2, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  lci3 <- as.numeric(summarise_draws(phi3, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci3 <- as.numeric(summarise_draws(phi3, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  lci4 <- as.numeric(summarise_draws(phi4, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci4 <- as.numeric(summarise_draws(phi4, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  lci5 <- as.numeric(summarise_draws(phi5, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci5 <- as.numeric(summarise_draws(phi5, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  lci6 <- as.numeric(summarise_draws(phi6, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci6 <- as.numeric(summarise_draws(phi6, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])

  
  
  cv_cumlogit_cpo1[k] <- ifelse(lci1 < log(1.10) & uci1 > log(1.10), 1, 0)
  cv_cumlogit_cpo2[k] <- ifelse(lci2 < log(1.10) & uci2 > log(1.10), 1, 0)
  cv_cumlogit_cpo3[k] <- ifelse(lci3 < log(1.10) & uci3 > log(1.10), 1, 0)
  cv_cumlogit_cpo4[k] <- ifelse(lci4 < log(1.10) & uci4 > log(1.10), 1, 0)
  cv_cumlogit_cpo5[k] <- ifelse(lci5 < log(1.10) & uci5 > log(1.10), 1, 0)
  cv_cumlogit_cpo6[k] <- ifelse(lci6 < log(1.10) & uci6 > log(1.10), 1, 0)


  
  # MSE 
  mse_cumlogit_cpo1[k] <- (as.numeric(summarise_draws(phi1, "median")[1,2]) - log(1.10))^2
  mse_cumlogit_cpo2[k] <- (as.numeric(summarise_draws(phi2, "median")[1,2]) - log(1.10))^2
  mse_cumlogit_cpo3[k] <- (as.numeric(summarise_draws(phi3, "median")[1,2]) - log(1.10))^2
  mse_cumlogit_cpo4[k] <- (as.numeric(summarise_draws(phi4, "median")[1,2]) - log(1.10))^2
  mse_cumlogit_cpo5[k] <- (as.numeric(summarise_draws(phi5, "median")[1,2]) - log(1.10))^2
  mse_cumlogit_cpo6[k] <- (as.numeric(summarise_draws(phi6, "median")[1,2]) - log(1.10))^2

  
  
  # MCSE 
  mcse_cumlogit_cpo1[k] <- as.numeric(summarise_draws(phi1, "mcse_median")[1,2])
  mcse_cumlogit_cpo2[k] <- as.numeric(summarise_draws(phi2, "mcse_median")[1,2])
  mcse_cumlogit_cpo3[k] <- as.numeric(summarise_draws(phi3, "mcse_median")[1,2])
  mcse_cumlogit_cpo4[k] <- as.numeric(summarise_draws(phi4, "mcse_median")[1,2])
  mcse_cumlogit_cpo5[k] <- as.numeric(summarise_draws(phi5, "mcse_median")[1,2])
  mcse_cumlogit_cpo6[k] <- as.numeric(summarise_draws(phi6, "mcse_median")[1,2])


  
  # R-hat 
  rhat_cumlogit_cpo1[k] <- as.numeric(summarise_draws(phi1, "rhat")[1,2])
  rhat_cumlogit_cpo2[k] <- as.numeric(summarise_draws(phi2, "rhat")[1,2])
  rhat_cumlogit_cpo3[k] <- as.numeric(summarise_draws(phi3, "rhat")[1,2])
  rhat_cumlogit_cpo4[k] <- as.numeric(summarise_draws(phi4, "rhat")[1,2])
  rhat_cumlogit_cpo5[k] <- as.numeric(summarise_draws(phi5, "rhat")[1,2])
  rhat_cumlogit_cpo6[k] <- as.numeric(summarise_draws(phi6, "rhat")[1,2])


  
  # Bulk ESS 
  bulkess_cumlogit_cpo1[k] <- as.numeric(summarise_draws(phi1, "ess_bulk")[1,2])
  bulkess_cumlogit_cpo2[k] <- as.numeric(summarise_draws(phi2, "ess_bulk")[1,2])
  bulkess_cumlogit_cpo3[k] <- as.numeric(summarise_draws(phi3, "ess_bulk")[1,2])
  bulkess_cumlogit_cpo4[k] <- as.numeric(summarise_draws(phi4, "ess_bulk")[1,2])
  bulkess_cumlogit_cpo5[k] <- as.numeric(summarise_draws(phi5, "ess_bulk")[1,2])
  bulkess_cumlogit_cpo6[k] <- as.numeric(summarise_draws(phi6, "ess_bulk")[1,2])


  
  # Tail ESS 
  tailess_cumlogit_cpo1[k] <- as.numeric(summarise_draws(phi1, "ess_tail")[1,2])
  tailess_cumlogit_cpo2[k] <- as.numeric(summarise_draws(phi2, "ess_tail")[1,2])
  tailess_cumlogit_cpo3[k] <- as.numeric(summarise_draws(phi3, "ess_tail")[1,2])
  tailess_cumlogit_cpo4[k] <- as.numeric(summarise_draws(phi4, "ess_tail")[1,2])
  tailess_cumlogit_cpo5[k] <- as.numeric(summarise_draws(phi5, "ess_tail")[1,2])
  tailess_cumlogit_cpo6[k] <- as.numeric(summarise_draws(phi6, "ess_tail")[1,2])


  # Number of divergent transitions 
  numdivergent_cpo[k] <- get_num_divergent(stanGet(cppomoddiv))
  
  
  # Number of missing values
  missing_cpo[k] <- length(cppomoddiv$na.action)
  
  
  ################### LOGISTIC MODEL - FIRST CUMULATIVE LOGIT ################
  logmod1 <- blrm(y1~x1, 
                  pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                 contrast=expression(c2-c1), sd=100),
                  conc = 1, seed=1234,iter=7500,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(logmod1$draws)

  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_cumlogit_lr1[k] <- as.numeric(summarise_draws(df, "median")[2,2]) - log(1.10)
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  cv_cumlogit_lr1[k] <- ifelse(lci < log(1.10) & uci > log(1.10), 1, 0)
  
  # MSE 
  mse_cumlogit_lr1[k] <- (as.numeric(summarise_draws(df, "median")[2,2]) - log(1.10))^2

  
  # MCSE 
  mcse_cumlogit_lr1[k] <- as.numeric(summarise_draws(df, "mcse_median")[2,2])

  
  # R-hat 
  rhat_cumlogit_lr1[k] <- as.numeric(summarise_draws(df, "rhat")[2,2])

  
  # Bulk ESS 
  bulkess_cumlogit_lr1[k] <- as.numeric(summarise_draws(df, "ess_bulk")[2,2])
  
  
  # Tail ESS 
  tailess_cumlogit_lr1[k] <- as.numeric(summarise_draws(df, "ess_tail")[2,2])

  
  # Number of divergent transitions 
  numdivergent_lr1[k] <- get_num_divergent(stanGet(logmod1))
  
  
  # Number of missing values
  missing_lr1[k] <- length(logmod1$na.action)
  
  
  ################### LOGISTIC MODEL - SECOND CUMULATIVE LOGIT  ################
  logmod2 <- blrm(y2~x1, 
                  pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                 contrast=expression(c2-c1), sd=100),
                  conc = 1, seed=1234,iter=7500,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(logmod2$draws)
  
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_cumlogit_lr2[k] <- as.numeric(summarise_draws(df, "median")[2,2]) - log(1.10)
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  cv_cumlogit_lr2[k] <- ifelse(lci < log(1.10) & uci > log(1.10), 1, 0)
  
  # MSE 
  mse_cumlogit_lr2[k] <- (as.numeric(summarise_draws(df, "median")[2,2]) - log(1.10))^2
  
  
  # MCSE 
  mcse_cumlogit_lr2[k] <- as.numeric(summarise_draws(df, "mcse_median")[2,2])
  
  
  # R-hat 
  rhat_cumlogit_lr2[k] <- as.numeric(summarise_draws(df, "rhat")[2,2])
  
  
  # Bulk ESS 
  bulkess_cumlogit_lr2[k] <- as.numeric(summarise_draws(df, "ess_bulk")[2,2])
  
  
  # Tail ESS 
  tailess_cumlogit_lr2[k] <- as.numeric(summarise_draws(df, "ess_tail")[2,2])
  
  
  # Number of divergent transitions 
  numdivergent_lr2[k] <- get_num_divergent(stanGet(logmod2))
  
  
  # Number of missing values
  missing_lr2[k] <- length(logmod2$na.action)
  
  
  ################### LOGISTIC MODEL - THIRD CUMULATIVE LOGIT  ################
  logmod3 <- blrm(y3~x1, 
                  pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                 contrast=expression(c2-c1), sd=100),
                  conc = 1, seed=1234,iter=7500,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(logmod3$draws)
  
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_cumlogit_lr3[k] <- as.numeric(summarise_draws(df, "median")[2,2]) - log(1.10)
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  cv_cumlogit_lr3[k] <- ifelse(lci < log(1.10) & uci > log(1.10), 1, 0)
  
  # MSE 
  mse_cumlogit_lr3[k] <- (as.numeric(summarise_draws(df, "median")[2,2]) - log(1.10))^2
  
  
  # MCSE 
  mcse_cumlogit_lr3[k] <- as.numeric(summarise_draws(df, "mcse_median")[2,2])
  
  
  # R-hat 
  rhat_cumlogit_lr3[k] <- as.numeric(summarise_draws(df, "rhat")[2,2])
  
  
  # Bulk ESS 
  bulkess_cumlogit_lr3[k] <- as.numeric(summarise_draws(df, "ess_bulk")[2,2])
  
  
  # Tail ESS 
  tailess_cumlogit_lr3[k] <- as.numeric(summarise_draws(df, "ess_tail")[2,2])
  
  
  # Number of divergent transitions 
  numdivergent_lr3[k] <- get_num_divergent(stanGet(logmod3))
  
  
  # Number of missing values
  missing_lr3[k] <- length(logmod3$na.action)
  
  
  
  ################### LOGISTIC MODEL - FOURTH CUMULATIVE LOGIT  ################
  logmod4 <- blrm(y4~x1, 
                  pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                 contrast=expression(c2-c1), sd=100),
                  conc = 1, seed=1234,iter=7500,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(logmod4$draws)
  
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_cumlogit_lr4[k] <- as.numeric(summarise_draws(df, "median")[2,2]) - log(1.10)
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  cv_cumlogit_lr4[k] <- ifelse(lci < log(1.10) & uci > log(1.10), 1, 0)
  
  # MSE 
  mse_cumlogit_lr4[k] <- (as.numeric(summarise_draws(df, "median")[2,2]) - log(1.10))^2
  
  
  # MCSE 
  mcse_cumlogit_lr4[k] <- as.numeric(summarise_draws(df, "mcse_median")[2,2])
  
  
  # R-hat 
  rhat_cumlogit_lr4[k] <- as.numeric(summarise_draws(df, "rhat")[2,2])
  
  
  # Bulk ESS
  bulkess_cumlogit_lr4[k] <- as.numeric(summarise_draws(df, "ess_bulk")[2,2])
  
  
  # Tail ESS 
  tailess_cumlogit_lr4[k] <- as.numeric(summarise_draws(df, "ess_tail")[2,2])
  
  
  # Number of divergent transitions 
  numdivergent_lr4[k] <- get_num_divergent(stanGet(logmod4))
  
  
  # Number of missing values
  missing_lr4[k] <- length(logmod4$na.action)
  
  
  ################### LOGISTIC MODEL - FIFTH CUMULATIVE LOGIT  ################
  logmod5 <- blrm(y5~x1, 
                  pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                 contrast=expression(c2-c1), sd=100),
                  conc = 1, seed=1234,iter=7500,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(logmod5$draws)
  
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_cumlogit_lr5[k] <- as.numeric(summarise_draws(df, "median")[2,2]) - log(1.10)
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  cv_cumlogit_lr5[k] <- ifelse(lci < log(1.10) & uci > log(1.10), 1, 0)
  
  # MSE 
  mse_cumlogit_lr5[k] <- (as.numeric(summarise_draws(df, "median")[2,2]) - log(1.10))^2
  
  
  # MCSE 
  mcse_cumlogit_lr5[k] <- as.numeric(summarise_draws(df, "mcse_median")[2,2])
  
  
  # R-hat 
  rhat_cumlogit_lr5[k] <- as.numeric(summarise_draws(df, "rhat")[2,2])
  
  
  # Bulk ESS 
  bulkess_cumlogit_lr5[k] <- as.numeric(summarise_draws(df, "ess_bulk")[2,2])
  
  
  # Tail ESS 
  tailess_cumlogit_lr5[k] <- as.numeric(summarise_draws(df, "ess_tail")[2,2])
  
  
  # Number of divergent transitions 
  numdivergent_lr5[k] <- get_num_divergent(stanGet(logmod5))
  
  
  # Number of missing values
  missing_lr5[k] <- length(logmod5$na.action)
  
  
  ################### LOGISTIC MODEL - SIXTH CUMULATIVE LOGIT  ################
  logmod6 <- blrm(y6~x1, 
                  pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                 contrast=expression(c2-c1), sd=100),
                  conc = 1, seed=1234,iter=7500,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(logmod6$draws)
  
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_cumlogit_lr6[k] <- as.numeric(summarise_draws(df, "median")[2,2]) - log(1.10)
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  cv_cumlogit_lr6[k] <- ifelse(lci < log(1.10) & uci > log(1.10), 1, 0)
  
  # MSE 
  mse_cumlogit_lr6[k] <- (as.numeric(summarise_draws(df, "median")[2,2]) - log(1.10))^2
  
  
  # MCSE 
  mcse_cumlogit_lr6[k] <- as.numeric(summarise_draws(df, "mcse_median")[2,2])
  
  
  # R-hat 
  rhat_cumlogit_lr6[k] <- as.numeric(summarise_draws(df, "rhat")[2,2])
  
  
  # Bulk ESS 
  bulkess_cumlogit_lr6[k] <- as.numeric(summarise_draws(df, "ess_bulk")[2,2])
  
  
  # Tail ESS 
  tailess_cumlogit_lr6[k] <- as.numeric(summarise_draws(df, "ess_tail")[2,2])
  
  
  # Number of divergent transitions 
  numdivergent_lr6[k] <- get_num_divergent(stanGet(logmod6))
  
  
  # Number of missing values
  missing_lr6[k] <- length(logmod6$na.action)
  
}



## Format data for analysis 
data <- data.frame(bias_cumlogit_cpo1 = bias_cumlogit_cpo1, bias_cumlogit_cpo2 = bias_cumlogit_cpo2,
                   bias_cumlogit_cpo3 = bias_cumlogit_cpo3, bias_cumlogit_cpo4 = bias_cumlogit_cpo4,
                   bias_cumlogit_cpo5 = bias_cumlogit_cpo5, bias_cumlogit_cpo6 = bias_cumlogit_cpo6,
                   bias_cumlogit_lcpo1 = bias_cumlogit_lcpo1,bias_cumlogit_lcpo2 = bias_cumlogit_lcpo2,
                   bias_cumlogit_lcpo3 = bias_cumlogit_lcpo3,bias_cumlogit_lcpo4 = bias_cumlogit_lcpo4,
                   bias_cumlogit_lcpo5 = bias_cumlogit_lcpo5,bias_cumlogit_lcpo6 = bias_cumlogit_lcpo6,
                   bias_cumlogit_lr1 = bias_cumlogit_lr1,bias_cumlogit_lr2 = bias_cumlogit_lr2,
                   bias_cumlogit_lr3 = bias_cumlogit_lr3,bias_cumlogit_lr4 = bias_cumlogit_lr4,
                   bias_cumlogit_lr5 = bias_cumlogit_lr5,bias_cumlogit_lr6 = bias_cumlogit_lr6,
                   bias_cumlogit_po1 = bias_cumlogit_po1,bias_cumlogit_po2 = bias_cumlogit_po2,
                   bias_cumlogit_po3 = bias_cumlogit_po3,bias_cumlogit_po4 = bias_cumlogit_po4,
                   bias_cumlogit_po5 = bias_cumlogit_po5,bias_cumlogit_po6 = bias_cumlogit_po6,
                   bias_cumlogit_ppo1 = bias_cumlogit_ppo1,bias_cumlogit_ppo2 = bias_cumlogit_ppo2,
                   bias_cumlogit_ppo3 = bias_cumlogit_ppo3,bias_cumlogit_ppo4 = bias_cumlogit_ppo4,
                   bias_cumlogit_ppo5 = bias_cumlogit_ppo5,bias_cumlogit_ppo6 = bias_cumlogit_ppo6,
                   cv_cumlogit_cpo1 = cv_cumlogit_cpo1, cv_cumlogit_cpo2 = cv_cumlogit_cpo2,
                   cv_cumlogit_cpo3 = cv_cumlogit_cpo3, cv_cumlogit_cpo4 = cv_cumlogit_cpo4,
                   cv_cumlogit_cpo5 = cv_cumlogit_cpo5, cv_cumlogit_cpo6 = cv_cumlogit_cpo6,
                   cv_cumlogit_lcpo1 = cv_cumlogit_lcpo1,cv_cumlogit_lcpo2 = cv_cumlogit_lcpo2,
                   cv_cumlogit_lcpo3 = cv_cumlogit_lcpo3,cv_cumlogit_lcpo4 = cv_cumlogit_lcpo4,
                   cv_cumlogit_lcpo5 = cv_cumlogit_lcpo5,cv_cumlogit_lcpo6 = cv_cumlogit_lcpo6,
                   cv_cumlogit_lr1 = cv_cumlogit_lr1,cv_cumlogit_lr2 = cv_cumlogit_lr2,
                   cv_cumlogit_lr3 = cv_cumlogit_lr3,cv_cumlogit_lr4 = cv_cumlogit_lr4,
                   cv_cumlogit_lr5 = cv_cumlogit_lr5,cv_cumlogit_lr6 = cv_cumlogit_lr6,
                   cv_cumlogit_po1 = cv_cumlogit_po1,cv_cumlogit_po2 = cv_cumlogit_po2,
                   cv_cumlogit_po3 = cv_cumlogit_po3,cv_cumlogit_po4 = cv_cumlogit_po4,
                   cv_cumlogit_po5 = cv_cumlogit_po5,cv_cumlogit_po6 = cv_cumlogit_po6,
                   cv_cumlogit_ppo1 = cv_cumlogit_ppo1,cv_cumlogit_ppo2 = cv_cumlogit_ppo2,
                   cv_cumlogit_ppo3 = cv_cumlogit_ppo3,cv_cumlogit_ppo4 = cv_cumlogit_ppo4,
                   cv_cumlogit_ppo5 = cv_cumlogit_ppo5,cv_cumlogit_ppo6 = cv_cumlogit_ppo6,
                   mcse_cumlogit_cpo1 = mcse_cumlogit_cpo1, mcse_cumlogit_cpo2 = mcse_cumlogit_cpo2,
                   mcse_cumlogit_cpo3 = mcse_cumlogit_cpo3, mcse_cumlogit_cpo4 = mcse_cumlogit_cpo4,
                   mcse_cumlogit_cpo5 = mcse_cumlogit_cpo5, mcse_cumlogit_cpo6 = mcse_cumlogit_cpo6,
                   mcse_cumlogit_lcpo1 = mcse_cumlogit_lcpo1,mcse_cumlogit_lcpo2 = mcse_cumlogit_lcpo2,
                   mcse_cumlogit_lcpo3 = mcse_cumlogit_lcpo3,mcse_cumlogit_lcpo4 = mcse_cumlogit_lcpo4,
                   mcse_cumlogit_lcpo5 = mcse_cumlogit_lcpo5,mcse_cumlogit_lcpo6 = mcse_cumlogit_lcpo6,
                   mcse_cumlogit_lr1 = mcse_cumlogit_lr1,mcse_cumlogit_lr2 = mcse_cumlogit_lr2,
                   mcse_cumlogit_lr3 = mcse_cumlogit_lr3,mcse_cumlogit_lr4 = mcse_cumlogit_lr4,
                   mcse_cumlogit_lr5 = mcse_cumlogit_lr5,mcse_cumlogit_lr6 = mcse_cumlogit_lr6,
                   mcse_cumlogit_po1 = mcse_cumlogit_po1,mcse_cumlogit_po2 = mcse_cumlogit_po2,
                   mcse_cumlogit_po3 = mcse_cumlogit_po3,mcse_cumlogit_po4 = mcse_cumlogit_po4,
                   mcse_cumlogit_po5 = mcse_cumlogit_po5,mcse_cumlogit_po6 = mcse_cumlogit_po6,
                   mcse_cumlogit_ppo1 = mcse_cumlogit_ppo1,mcse_cumlogit_ppo2 = mcse_cumlogit_ppo2,
                   mcse_cumlogit_ppo3 = mcse_cumlogit_ppo3,mcse_cumlogit_ppo4 = mcse_cumlogit_ppo4,
                   mcse_cumlogit_ppo5 = mcse_cumlogit_ppo5,mcse_cumlogit_ppo6 = mcse_cumlogit_ppo6,
                   bulkess_cumlogit_cpo1 = bulkess_cumlogit_cpo1, bulkess_cumlogit_cpo2 = bulkess_cumlogit_cpo2,
                   bulkess_cumlogit_cpo3 = bulkess_cumlogit_cpo3, bulkess_cumlogit_cpo4 = bulkess_cumlogit_cpo4,
                   bulkess_cumlogit_cpo5 = bulkess_cumlogit_cpo5, bulkess_cumlogit_cpo6 = bulkess_cumlogit_cpo6,
                   bulkess_cumlogit_lcpo1 = bulkess_cumlogit_lcpo1,bulkess_cumlogit_lcpo2 = bulkess_cumlogit_lcpo2,
                   bulkess_cumlogit_lcpo3 = bulkess_cumlogit_lcpo3,bulkess_cumlogit_lcpo4 = bulkess_cumlogit_lcpo4,
                   bulkess_cumlogit_lcpo5 = bulkess_cumlogit_lcpo5,bulkess_cumlogit_lcpo6 = bulkess_cumlogit_lcpo6,
                   bulkess_cumlogit_lr1 = bulkess_cumlogit_lr1,bulkess_cumlogit_lr2 = bulkess_cumlogit_lr2,
                   bulkess_cumlogit_lr3 = bulkess_cumlogit_lr3,bulkess_cumlogit_lr4 = bulkess_cumlogit_lr4,
                   bulkess_cumlogit_lr5 = bulkess_cumlogit_lr5,bulkess_cumlogit_lr6 = bulkess_cumlogit_lr6,
                   bulkess_cumlogit_po1 = bulkess_cumlogit_po1,bulkess_cumlogit_po2 = bulkess_cumlogit_po2,
                   bulkess_cumlogit_po3 = bulkess_cumlogit_po3,bulkess_cumlogit_po4 = bulkess_cumlogit_po4,
                   bulkess_cumlogit_po5 = bulkess_cumlogit_po5,bulkess_cumlogit_po6 = bulkess_cumlogit_po6,
                   bulkess_cumlogit_ppo1 = bulkess_cumlogit_ppo1,bulkess_cumlogit_ppo2 = bulkess_cumlogit_ppo2,
                   bulkess_cumlogit_ppo3 = bulkess_cumlogit_ppo3,bulkess_cumlogit_ppo4 = bulkess_cumlogit_ppo4,
                   bulkess_cumlogit_ppo5 = bulkess_cumlogit_ppo5,bulkess_cumlogit_ppo6 = bulkess_cumlogit_ppo6,
                   tailess_cumlogit_cpo1 = tailess_cumlogit_cpo1, tailess_cumlogit_cpo2 = tailess_cumlogit_cpo2,
                   tailess_cumlogit_cpo3 = tailess_cumlogit_cpo3, tailess_cumlogit_cpo4 = tailess_cumlogit_cpo4,
                   tailess_cumlogit_cpo5 = tailess_cumlogit_cpo5, tailess_cumlogit_cpo6 = tailess_cumlogit_cpo6,
                   tailess_cumlogit_lcpo1 = tailess_cumlogit_lcpo1,tailess_cumlogit_lcpo2 = tailess_cumlogit_lcpo2,
                   tailess_cumlogit_lcpo3 = tailess_cumlogit_lcpo3,tailess_cumlogit_lcpo4 = tailess_cumlogit_lcpo4,
                   tailess_cumlogit_lcpo5 = tailess_cumlogit_lcpo5,tailess_cumlogit_lcpo6 = tailess_cumlogit_lcpo6,
                   tailess_cumlogit_lr1 = tailess_cumlogit_lr1,tailess_cumlogit_lr2 = tailess_cumlogit_lr2,
                   tailess_cumlogit_lr3 = tailess_cumlogit_lr3,tailess_cumlogit_lr4 = tailess_cumlogit_lr4,
                   tailess_cumlogit_lr5 = tailess_cumlogit_lr5,tailess_cumlogit_lr6 = tailess_cumlogit_lr6,
                   tailess_cumlogit_po1 = tailess_cumlogit_po1,tailess_cumlogit_po2 = tailess_cumlogit_po2,
                   tailess_cumlogit_po3 = tailess_cumlogit_po3,tailess_cumlogit_po4 = tailess_cumlogit_po4,
                   tailess_cumlogit_po5 = tailess_cumlogit_po5,tailess_cumlogit_po6 = tailess_cumlogit_po6,
                   tailess_cumlogit_ppo1 = tailess_cumlogit_ppo1,tailess_cumlogit_ppo2 = tailess_cumlogit_ppo2,
                   tailess_cumlogit_ppo3 = tailess_cumlogit_ppo3,tailess_cumlogit_ppo4 = tailess_cumlogit_ppo4,
                   tailess_cumlogit_ppo5 = tailess_cumlogit_ppo5,tailess_cumlogit_ppo6 = tailess_cumlogit_ppo6,
                   mse_cumlogit_cpo1 = mse_cumlogit_cpo1, mse_cumlogit_cpo2 = mse_cumlogit_cpo2,
                   mse_cumlogit_cpo3 = mse_cumlogit_cpo3, mse_cumlogit_cpo4 = mse_cumlogit_cpo4,
                   mse_cumlogit_cpo5 = mse_cumlogit_cpo5, mse_cumlogit_cpo6 = mse_cumlogit_cpo6,
                   mse_cumlogit_lcpo1 = mse_cumlogit_lcpo1,mse_cumlogit_lcpo2 = mse_cumlogit_lcpo2,
                   mse_cumlogit_lcpo3 = mse_cumlogit_lcpo3,mse_cumlogit_lcpo4 = mse_cumlogit_lcpo4,
                   mse_cumlogit_lcpo5 = mse_cumlogit_lcpo5,mse_cumlogit_lcpo6 = mse_cumlogit_lcpo6,
                   mse_cumlogit_lr1 = mse_cumlogit_lr1,mse_cumlogit_lr2 = mse_cumlogit_lr2,
                   mse_cumlogit_lr3 = mse_cumlogit_lr3,mse_cumlogit_lr4 = mse_cumlogit_lr4,
                   mse_cumlogit_lr5 = mse_cumlogit_lr5,mse_cumlogit_lr6 = mse_cumlogit_lr6,
                   mse_cumlogit_po1 = mse_cumlogit_po1,mse_cumlogit_po2 = mse_cumlogit_po2,
                   mse_cumlogit_po3 = mse_cumlogit_po3,mse_cumlogit_po4 = mse_cumlogit_po4,
                   mse_cumlogit_po5 = mse_cumlogit_po5,mse_cumlogit_po6 = mse_cumlogit_po6,
                   mse_cumlogit_ppo1 = mse_cumlogit_ppo1,mse_cumlogit_ppo2 = mse_cumlogit_ppo2,
                   mse_cumlogit_ppo3 = mse_cumlogit_ppo3,mse_cumlogit_ppo4 = mse_cumlogit_ppo4,
                   mse_cumlogit_ppo5 = mse_cumlogit_ppo5,mse_cumlogit_ppo6 = mse_cumlogit_ppo6,
                   rhat_cumlogit_cpo1 = rhat_cumlogit_cpo1, rhat_cumlogit_cpo2 = rhat_cumlogit_cpo2,
                   rhat_cumlogit_cpo3 = rhat_cumlogit_cpo3, rhat_cumlogit_cpo4 = rhat_cumlogit_cpo4,
                   rhat_cumlogit_cpo5 = rhat_cumlogit_cpo5, rhat_cumlogit_cpo6 = rhat_cumlogit_cpo6,
                   rhat_cumlogit_lcpo1 = rhat_cumlogit_lcpo1,rhat_cumlogit_lcpo2 = rhat_cumlogit_lcpo2,
                   rhat_cumlogit_lcpo3 = rhat_cumlogit_lcpo3,rhat_cumlogit_lcpo4 = rhat_cumlogit_lcpo4,
                   rhat_cumlogit_lcpo5 = rhat_cumlogit_lcpo5,rhat_cumlogit_lcpo6 = rhat_cumlogit_lcpo6,
                   rhat_cumlogit_lr1 = rhat_cumlogit_lr1,rhat_cumlogit_lr2 = rhat_cumlogit_lr2,
                   rhat_cumlogit_lr3 = rhat_cumlogit_lr3,rhat_cumlogit_lr4 = rhat_cumlogit_lr4,
                   rhat_cumlogit_lr5 = rhat_cumlogit_lr5,rhat_cumlogit_lr6 = rhat_cumlogit_lr6,
                   rhat_cumlogit_po1 = rhat_cumlogit_po1,rhat_cumlogit_po2 = rhat_cumlogit_po2,
                   rhat_cumlogit_po3 = rhat_cumlogit_po3,rhat_cumlogit_po4 = rhat_cumlogit_po4,
                   rhat_cumlogit_po5 = rhat_cumlogit_po5,rhat_cumlogit_po6 = rhat_cumlogit_po6,
                   rhat_cumlogit_ppo1 = rhat_cumlogit_ppo1,rhat_cumlogit_ppo2 = rhat_cumlogit_ppo2,
                   rhat_cumlogit_ppo3 = rhat_cumlogit_ppo3,rhat_cumlogit_ppo4 = rhat_cumlogit_ppo4,
                   rhat_cumlogit_ppo5 = rhat_cumlogit_ppo5,rhat_cumlogit_ppo6 = rhat_cumlogit_ppo6,
                   missing_cpo = missing_cpo, missing_lcpo = missing_lcpo,
                   missing_lr1 = missing_lr1,missing_lr2 = missing_lr2,
                   missing_lr3 = missing_lr3,missing_lr4 = missing_lr4,
                   missing_lr5 = missing_lr5,missing_lr6 = missing_lr6,
                   missing_po = missing_po,missing_ppo = missing_ppo,
                   numdivergent_cpo = numdivergent_cpo, numdivergent_lcpo = numdivergent_lcpo,
                   numdivergent_lr1 = numdivergent_lr1,numdivergent_lr2 = numdivergent_lr2,
                   numdivergent_lr3 = numdivergent_lr3,numdivergent_lr4 = numdivergent_lr4,
                   numdivergent_lr5 = numdivergent_lr5,numdivergent_lr6 = numdivergent_lr6,
                   numdivergent_po = numdivergent_po,numdivergent_ppo = numdivergent_ppo
)  


data <- data[s1[datnum]:s2[datnum],]

save(data, file = paste0("largen_sym_7cat_randompo_smalleff_none",datnum,".Rdata"))

end <- Sys.time()
start-end


