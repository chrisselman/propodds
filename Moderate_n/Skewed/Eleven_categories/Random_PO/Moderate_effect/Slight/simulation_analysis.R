## Set working directory 
setwd("/group/cebu1/BACKUP/Chris/Project_2/Main_Simulations/Moderate_n/Skewed/Eleven_categories/Random_PO/Moderate_effect/Slight")


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
load("smalln_skew_11cat_randompo_modeff_slight_dataset.Rdata")
attach(big_data)

# Select a subset of the data
s2 = seq(5,1000,by=5)
s1=s2-4

temp = big_data[(big_data$k >= s1[datnum] & big_data$k <= s2[datnum]),]

# Set seed
set.seed(071023)
seed=sample(1:1e8,200,replace=F)[datnum]

set.seed(seed)
nsim <- 3
states <- c("A", "B", "C","D","E","F","G","H","I","J", "K")

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
missing_po <- missing_ppo <- missing_cpo <- missing_lcpo <- missing_lr1 <- missing_lr2 <- missing_lr3 <- missing_lr4 <- missing_lr5 <- missing_lr6 <- missing_lr7 <- missing_lr8 <- missing_lr9 <- missing_lr10 <- length(nsim) # to store number of missing values in each model 


## Number of divergent transitions 
numdivergent_po <- numdivergent_ppo <- numdivergent_cpo <- numdivergent_lcpo <- numdivergent_lr1 <- numdivergent_lr2 <- numdivergent_lr3 <- numdivergent_lr4 <- numdivergent_lr5 <- numdivergent_lr6 <- numdivergent_lr7 <- numdivergent_lr8 <- numdivergent_lr9 <- numdivergent_lr10 <- length(nsim) # to store number of divergent transitions in each model 


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
  y7 <- big_data$y7[big_data$k == k]
  y8 <- big_data$y8[big_data$k == k]
  y9 <- big_data$y9[big_data$k == k]
  y10 <- big_data$y10[big_data$k == k]
  
  
  ##################### Proportional odds model ##################
  pomod <- blrm(y~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                  contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=7500,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(pomod$draws)
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_cumlogit_po1[k] <- as.numeric(summarise_draws(df, "median")[11,2]) - log(1.50)
  bias_cumlogit_po2[k] <- as.numeric(summarise_draws(df, "median")[11,2]) - log(1.50)
  bias_cumlogit_po3[k] <- as.numeric(summarise_draws(df, "median")[11,2]) - log(1.50)
  bias_cumlogit_po4[k] <- as.numeric(summarise_draws(df, "median")[11,2]) - log(1.50)
  bias_cumlogit_po5[k] <- as.numeric(summarise_draws(df, "median")[11,2]) - log(1.50)
  bias_cumlogit_po6[k] <- as.numeric(summarise_draws(df, "median")[11,2]) - log(1.50)
  bias_cumlogit_po7[k] <- as.numeric(summarise_draws(df, "median")[11,2]) - log(1.50)
  bias_cumlogit_po8[k] <- as.numeric(summarise_draws(df, "median")[11,2]) - log(1.50)
  bias_cumlogit_po9[k] <- as.numeric(summarise_draws(df, "median")[11,2]) - log(1.50)
  bias_cumlogit_po10[k] <- as.numeric(summarise_draws(df, "median")[11,2]) - log(1.50)

  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[11,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[11,3])

  cv_cumlogit_po1[k] <- ifelse(lci < log(1.50) & uci > log(1.50), 1, 0)
  cv_cumlogit_po2[k] <- ifelse(lci < log(1.50) & uci > log(1.50), 1, 0)
  cv_cumlogit_po3[k] <- ifelse(lci < log(1.50) & uci > log(1.50), 1, 0)
  cv_cumlogit_po4[k] <- ifelse(lci < log(1.50) & uci > log(1.50), 1, 0)
  cv_cumlogit_po5[k] <- ifelse(lci < log(1.50) & uci > log(1.50), 1, 0)
  cv_cumlogit_po6[k] <- ifelse(lci < log(1.50) & uci > log(1.50), 1, 0)
  cv_cumlogit_po7[k] <- ifelse(lci < log(1.50) & uci > log(1.50), 1, 0)
  cv_cumlogit_po8[k] <- ifelse(lci < log(1.50) & uci > log(1.50), 1, 0)
  cv_cumlogit_po9[k] <- ifelse(lci < log(1.50) & uci > log(1.50), 1, 0)
  cv_cumlogit_po10[k] <- ifelse(lci < log(1.50) & uci > log(1.50), 1, 0)

  
  # MSE 
  mse_cumlogit_po1[k] <- (as.numeric(summarise_draws(df, "median")[11,2]) - log(1.50))^2
  mse_cumlogit_po2[k] <- (as.numeric(summarise_draws(df, "median")[11,2]) - log(1.50))^2
  mse_cumlogit_po3[k] <- (as.numeric(summarise_draws(df, "median")[11,2]) - log(1.50))^2
  mse_cumlogit_po4[k] <- (as.numeric(summarise_draws(df, "median")[11,2]) - log(1.50))^2
  mse_cumlogit_po5[k] <- (as.numeric(summarise_draws(df, "median")[11,2]) - log(1.50))^2
  mse_cumlogit_po6[k] <- (as.numeric(summarise_draws(df, "median")[11,2]) - log(1.50))^2
  mse_cumlogit_po7[k] <- (as.numeric(summarise_draws(df, "median")[11,2]) - log(1.50))^2
  mse_cumlogit_po8[k] <- (as.numeric(summarise_draws(df, "median")[11,2]) - log(1.50))^2
  mse_cumlogit_po9[k] <- (as.numeric(summarise_draws(df, "median")[11,2]) - log(1.50))^2
  mse_cumlogit_po10[k] <- (as.numeric(summarise_draws(df, "median")[11,2]) - log(1.50))^2
  
  # MCSE 
  mcse_cumlogit_po1[k] <- as.numeric(summarise_draws(df, "mcse_median")[11,2])
  mcse_cumlogit_po2[k] <- as.numeric(summarise_draws(df, "mcse_median")[11,2])
  mcse_cumlogit_po3[k] <- as.numeric(summarise_draws(df, "mcse_median")[11,2])
  mcse_cumlogit_po4[k] <- as.numeric(summarise_draws(df, "mcse_median")[11,2])
  mcse_cumlogit_po5[k] <- as.numeric(summarise_draws(df, "mcse_median")[11,2])
  mcse_cumlogit_po6[k] <- as.numeric(summarise_draws(df, "mcse_median")[11,2])
  mcse_cumlogit_po7[k] <- as.numeric(summarise_draws(df, "mcse_median")[11,2])
  mcse_cumlogit_po8[k] <- as.numeric(summarise_draws(df, "mcse_median")[11,2])
  mcse_cumlogit_po9[k] <- as.numeric(summarise_draws(df, "mcse_median")[11,2])
  mcse_cumlogit_po10[k] <- as.numeric(summarise_draws(df, "mcse_median")[11,2])

  
  # R-hat 
  rhat_cumlogit_po1[k] <- as.numeric(summarise_draws(df, "rhat")[11,2])
  rhat_cumlogit_po2[k] <- as.numeric(summarise_draws(df, "rhat")[11,2])
  rhat_cumlogit_po3[k] <- as.numeric(summarise_draws(df, "rhat")[11,2])
  rhat_cumlogit_po4[k] <- as.numeric(summarise_draws(df, "rhat")[11,2])
  rhat_cumlogit_po5[k] <- as.numeric(summarise_draws(df, "rhat")[11,2])
  rhat_cumlogit_po6[k] <- as.numeric(summarise_draws(df, "rhat")[11,2])
  rhat_cumlogit_po7[k] <- as.numeric(summarise_draws(df, "rhat")[11,2])
  rhat_cumlogit_po8[k] <- as.numeric(summarise_draws(df, "rhat")[11,2])
  rhat_cumlogit_po9[k] <- as.numeric(summarise_draws(df, "rhat")[11,2])
  rhat_cumlogit_po10[k] <- as.numeric(summarise_draws(df, "rhat")[11,2])

  
  # Bulk ESS 
  bulkess_cumlogit_po1[k] <- as.numeric(summarise_draws(df, "ess_bulk")[11,2])
  bulkess_cumlogit_po2[k] <- as.numeric(summarise_draws(df, "ess_bulk")[11,2])
  bulkess_cumlogit_po3[k] <- as.numeric(summarise_draws(df, "ess_bulk")[11,2])
  bulkess_cumlogit_po4[k] <- as.numeric(summarise_draws(df, "ess_bulk")[11,2])
  bulkess_cumlogit_po5[k] <- as.numeric(summarise_draws(df, "ess_bulk")[11,2])
  bulkess_cumlogit_po6[k] <- as.numeric(summarise_draws(df, "ess_bulk")[11,2])
  bulkess_cumlogit_po7[k] <- as.numeric(summarise_draws(df, "ess_bulk")[11,2])
  bulkess_cumlogit_po8[k] <- as.numeric(summarise_draws(df, "ess_bulk")[11,2])
  bulkess_cumlogit_po9[k] <- as.numeric(summarise_draws(df, "ess_bulk")[11,2])
  bulkess_cumlogit_po10[k] <- as.numeric(summarise_draws(df, "ess_bulk")[11,2])

  
  # Tail ESS 
  tailess_cumlogit_po1[k] <- as.numeric(summarise_draws(df, "ess_tail")[11,2])
  tailess_cumlogit_po2[k] <- as.numeric(summarise_draws(df, "ess_tail")[11,2])
  tailess_cumlogit_po3[k] <- as.numeric(summarise_draws(df, "ess_tail")[11,2])
  tailess_cumlogit_po4[k] <- as.numeric(summarise_draws(df, "ess_tail")[11,2])
  tailess_cumlogit_po5[k] <- as.numeric(summarise_draws(df, "ess_tail")[11,2])
  tailess_cumlogit_po6[k] <- as.numeric(summarise_draws(df, "ess_tail")[11,2])
  tailess_cumlogit_po7[k] <- as.numeric(summarise_draws(df, "ess_tail")[11,2])
  tailess_cumlogit_po8[k] <- as.numeric(summarise_draws(df, "ess_tail")[11,2])
  tailess_cumlogit_po9[k] <- as.numeric(summarise_draws(df, "ess_tail")[11,2])
  tailess_cumlogit_po10[k] <- as.numeric(summarise_draws(df, "ess_tail")[11,2])
 
  
  # Number of divergent transitions 
  numdivergent_po[k] <- get_num_divergent(stanGet(pomod))
  
  
  # Number of missing values
  missing_po[k] <- length(pomod$na.action)
  
  
  ############### Unconstrained partial proportional odds model ###################
  ppomod <- blrm(y~x,ppo = ~x, priorsdppo = 100,
                 conc = 1, seed=1234,iter=7500,chains=4,sampling.args = list(cores = 4,control=list(adapt_delta=0.99,max_treedepth=12)))
  
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
  
  phi6 <- mutate_variables(df, phi = df$x + df$`x:y>=8`)
  phi6 <- subset_draws(phi6, c("phi"))
  
  phi7 <- mutate_variables(df, phi = df$x + df$`x:y>=9`)
  phi7 <- subset_draws(phi7, c("phi"))
  
  phi8 <- mutate_variables(df, phi = df$x + df$`x:y>=10`)
  phi8 <- subset_draws(phi8, c("phi"))
  
  phi9 <- mutate_variables(df, phi = df$x + df$`x:y>=11`)
  phi9 <- subset_draws(phi9, c("phi"))
  

  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_cumlogit_ppo1[k] <- as.numeric(summarise_draws(df, "median")[11,2]) - log(1.50)
  bias_cumlogit_ppo2[k] <- as.numeric(summarise_draws(phi1, "median")[1,2]) - log(1.50)
  bias_cumlogit_ppo3[k] <- as.numeric(summarise_draws(phi2, "median")[1,2]) - log(1.50)
  bias_cumlogit_ppo4[k] <- as.numeric(summarise_draws(phi3, "median")[1,2]) - log(1.50)
  bias_cumlogit_ppo5[k] <- as.numeric(summarise_draws(phi4, "median")[1,2]) - log(1.50)
  bias_cumlogit_ppo6[k] <- as.numeric(summarise_draws(phi5, "median")[1,2]) - log(1.50)
  bias_cumlogit_ppo7[k] <- as.numeric(summarise_draws(phi6, "median")[1,2]) - log(1.50)
  bias_cumlogit_ppo8[k] <- as.numeric(summarise_draws(phi7, "median")[1,2]) - log(1.50)
  bias_cumlogit_ppo9[k] <- as.numeric(summarise_draws(phi8, "median")[1,2]) - log(1.50)
  bias_cumlogit_ppo10[k] <- as.numeric(summarise_draws(phi9, "median")[1,2]) - log(1.50)

  
  # Coverage - does the true value fall in the credible interval?
  lci1 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[11,2])
  uci1 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[11,3])
  
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
  
  lci7 <- as.numeric(summarise_draws(phi6, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci7 <- as.numeric(summarise_draws(phi6, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  lci8 <- as.numeric(summarise_draws(phi7, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci8 <- as.numeric(summarise_draws(phi7, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  lci9 <- as.numeric(summarise_draws(phi8, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci9 <- as.numeric(summarise_draws(phi8, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  lci10 <- as.numeric(summarise_draws(phi9, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci10 <- as.numeric(summarise_draws(phi9, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  

  
  cv_cumlogit_ppo1[k] <- ifelse(lci1 < log(1.50) & uci1 > log(1.50), 1, 0)
  cv_cumlogit_ppo2[k] <- ifelse(lci2 < log(1.50) & uci2 > log(1.50), 1, 0)
  cv_cumlogit_ppo3[k] <- ifelse(lci3 < log(1.50) & uci3 > log(1.50), 1, 0)
  cv_cumlogit_ppo4[k] <- ifelse(lci4 < log(1.50) & uci4 > log(1.50), 1, 0)
  cv_cumlogit_ppo5[k] <- ifelse(lci5 < log(1.50) & uci5 > log(1.50), 1, 0)
  cv_cumlogit_ppo6[k] <- ifelse(lci6 < log(1.50) & uci6 > log(1.50), 1, 0)
  cv_cumlogit_ppo7[k] <- ifelse(lci7 < log(1.50) & uci7 > log(1.50), 1, 0)
  cv_cumlogit_ppo8[k] <- ifelse(lci8 < log(1.50) & uci8 > log(1.50), 1, 0)
  cv_cumlogit_ppo9[k] <- ifelse(lci9 < log(1.50) & uci9 > log(1.50), 1, 0)
  cv_cumlogit_ppo10[k] <- ifelse(lci10 < log(1.50) & uci10 > log(1.50), 1, 0)

  
  # MSE 
  mse_cumlogit_ppo1[k] <- (as.numeric(summarise_draws(df, "median")[11,2]) - log(1.50))^2
  mse_cumlogit_ppo2[k] <- (as.numeric(summarise_draws(phi1, "median")[1,2]) - log(1.50))^2
  mse_cumlogit_ppo3[k] <- (as.numeric(summarise_draws(phi2, "median")[1,2]) - log(1.50))^2
  mse_cumlogit_ppo4[k] <- (as.numeric(summarise_draws(phi3, "median")[1,2]) - log(1.50))^2
  mse_cumlogit_ppo5[k] <- (as.numeric(summarise_draws(phi4, "median")[1,2]) - log(1.50))^2
  mse_cumlogit_ppo6[k] <- (as.numeric(summarise_draws(phi5, "median")[1,2]) - log(1.50))^2
  mse_cumlogit_ppo7[k] <- (as.numeric(summarise_draws(phi6, "median")[1,2]) - log(1.50))^2
  mse_cumlogit_ppo8[k] <- (as.numeric(summarise_draws(phi7, "median")[1,2]) - log(1.50))^2
  mse_cumlogit_ppo9[k] <- (as.numeric(summarise_draws(phi8, "median")[1,2]) - log(1.50))^2
  mse_cumlogit_ppo10[k] <- (as.numeric(summarise_draws(phi9, "median")[1,2]) - log(1.50))^2

  
  # MCSE 
  mcse_cumlogit_ppo1[k] <- as.numeric(summarise_draws(df, "mcse_median")[11,2])
  mcse_cumlogit_ppo2[k] <- as.numeric(summarise_draws(phi1, "mcse_median")[1,2])
  mcse_cumlogit_ppo3[k] <- as.numeric(summarise_draws(phi2, "mcse_median")[1,2])
  mcse_cumlogit_ppo4[k] <- as.numeric(summarise_draws(phi3, "mcse_median")[1,2])
  mcse_cumlogit_ppo5[k] <- as.numeric(summarise_draws(phi4, "mcse_median")[1,2])
  mcse_cumlogit_ppo6[k] <- as.numeric(summarise_draws(phi5, "mcse_median")[1,2])
  mcse_cumlogit_ppo7[k] <- as.numeric(summarise_draws(phi6, "mcse_median")[1,2])
  mcse_cumlogit_ppo8[k] <- as.numeric(summarise_draws(phi7, "mcse_median")[1,2])
  mcse_cumlogit_ppo9[k] <- as.numeric(summarise_draws(phi8, "mcse_median")[1,2])
  mcse_cumlogit_ppo10[k] <- as.numeric(summarise_draws(phi9, "mcse_median")[1,2])

  
  # R-hat 
  rhat_cumlogit_ppo1[k] <- as.numeric(summarise_draws(df, "rhat")[11,2])
  rhat_cumlogit_ppo2[k] <- as.numeric(summarise_draws(phi1, "rhat")[1,2])
  rhat_cumlogit_ppo3[k] <- as.numeric(summarise_draws(phi2, "rhat")[1,2])
  rhat_cumlogit_ppo4[k] <- as.numeric(summarise_draws(phi3, "rhat")[1,2])
  rhat_cumlogit_ppo5[k] <- as.numeric(summarise_draws(phi4, "rhat")[1,2])
  rhat_cumlogit_ppo6[k] <- as.numeric(summarise_draws(phi5, "rhat")[1,2])
  rhat_cumlogit_ppo7[k] <- as.numeric(summarise_draws(phi6, "rhat")[1,2])
  rhat_cumlogit_ppo8[k] <- as.numeric(summarise_draws(phi7, "rhat")[1,2])
  rhat_cumlogit_ppo9[k] <- as.numeric(summarise_draws(phi8, "rhat")[1,2])
  rhat_cumlogit_ppo10[k] <- as.numeric(summarise_draws(phi9, "rhat")[1,2])

  
  # Bulk ESS 
  bulkess_cumlogit_ppo1[k] <- as.numeric(summarise_draws(df, "ess_bulk")[11,2])
  bulkess_cumlogit_ppo2[k] <- as.numeric(summarise_draws(phi1, "ess_bulk")[1,2])
  bulkess_cumlogit_ppo3[k] <- as.numeric(summarise_draws(phi2, "ess_bulk")[1,2])
  bulkess_cumlogit_ppo4[k] <- as.numeric(summarise_draws(phi3, "ess_bulk")[1,2])
  bulkess_cumlogit_ppo5[k] <- as.numeric(summarise_draws(phi4, "ess_bulk")[1,2])
  bulkess_cumlogit_ppo6[k] <- as.numeric(summarise_draws(phi5, "ess_bulk")[1,2])
  bulkess_cumlogit_ppo7[k] <- as.numeric(summarise_draws(phi6, "ess_bulk")[1,2])
  bulkess_cumlogit_ppo8[k] <- as.numeric(summarise_draws(phi7, "ess_bulk")[1,2])
  bulkess_cumlogit_ppo9[k] <- as.numeric(summarise_draws(phi8, "ess_bulk")[1,2])
  bulkess_cumlogit_ppo10[k] <- as.numeric(summarise_draws(phi9, "ess_bulk")[1,2])

  
  # Tail ESS 
  tailess_cumlogit_ppo1[k] <- as.numeric(summarise_draws(df, "ess_tail")[11,2])
  tailess_cumlogit_ppo2[k] <- as.numeric(summarise_draws(phi1, "ess_tail")[1,2])
  tailess_cumlogit_ppo3[k] <- as.numeric(summarise_draws(phi2, "ess_tail")[1,2])
  tailess_cumlogit_ppo4[k] <- as.numeric(summarise_draws(phi3, "ess_tail")[1,2])
  tailess_cumlogit_ppo5[k] <- as.numeric(summarise_draws(phi4, "ess_tail")[1,2])
  tailess_cumlogit_ppo6[k] <- as.numeric(summarise_draws(phi5, "ess_tail")[1,2])
  tailess_cumlogit_ppo7[k] <- as.numeric(summarise_draws(phi6, "ess_tail")[1,2])
  tailess_cumlogit_ppo8[k] <- as.numeric(summarise_draws(phi7, "ess_tail")[1,2])
  tailess_cumlogit_ppo9[k] <- as.numeric(summarise_draws(phi8, "ess_tail")[1,2])
  tailess_cumlogit_ppo10[k] <- as.numeric(summarise_draws(phi9, "ess_tail")[1,2])

  
  # Number of divergent transitions 
  numdivergent_ppo[k] <- get_num_divergent(stanGet(ppomod))
  
  
  # Number of missing values
  missing_ppo[k] <- length(ppomod$na.action)
  
  
  ################### CONSTRAINED PPO MODEL ASSUMING LINEARITY ################
  cppomodlin <- blrm(y~x,ppo = ~x, normcppo=T,
                     priorsdppo = 100,
                     conc = 1, seed=1234,iter=7500,chains=4,cppo=function(y) y,
                     sampling.args = list(cores = 4,control=list(adapt_delta=0.95,max_treedepth=12)))

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
  
  phi7 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(8))
  phi7 <- subset_draws(phi7, c("phi"))
  
  phi8 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(9))
  phi8 <- subset_draws(phi8, c("phi"))
  
  phi9 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(10))
  phi9 <- subset_draws(phi9, c("phi"))
  
  phi10 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(11))
  phi10 <- subset_draws(phi10, c("phi"))
  

  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_cumlogit_lcpo1[k] <- as.numeric(summarise_draws(phi1, "median")[1,2]) - log(1.50)
  bias_cumlogit_lcpo2[k] <- as.numeric(summarise_draws(phi2, "median")[1,2]) - log(1.50)
  bias_cumlogit_lcpo3[k] <- as.numeric(summarise_draws(phi3, "median")[1,2]) - log(1.50)
  bias_cumlogit_lcpo4[k] <- as.numeric(summarise_draws(phi4, "median")[1,2]) - log(1.50)
  bias_cumlogit_lcpo5[k] <- as.numeric(summarise_draws(phi5, "median")[1,2]) - log(1.50)
  bias_cumlogit_lcpo6[k] <- as.numeric(summarise_draws(phi6, "median")[1,2]) - log(1.50)
  bias_cumlogit_lcpo7[k] <- as.numeric(summarise_draws(phi7, "median")[1,2]) - log(1.50)
  bias_cumlogit_lcpo8[k] <- as.numeric(summarise_draws(phi8, "median")[1,2]) - log(1.50)
  bias_cumlogit_lcpo9[k] <- as.numeric(summarise_draws(phi9, "median")[1,2]) - log(1.50)
  bias_cumlogit_lcpo10[k] <- as.numeric(summarise_draws(phi10, "median")[1,2]) - log(1.50)

  
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
  
  lci7 <- as.numeric(summarise_draws(phi7, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci7 <- as.numeric(summarise_draws(phi7, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  lci8 <- as.numeric(summarise_draws(phi8, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci8 <- as.numeric(summarise_draws(phi8, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  lci9 <- as.numeric(summarise_draws(phi9, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci9 <- as.numeric(summarise_draws(phi9, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  lci10 <- as.numeric(summarise_draws(phi10, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci10 <- as.numeric(summarise_draws(phi10, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  
  cv_cumlogit_lcpo1[k] <- ifelse(lci1 < log(1.50) & uci1 > log(1.50), 1, 0)
  cv_cumlogit_lcpo2[k] <- ifelse(lci2 < log(1.50) & uci2 > log(1.50), 1, 0)
  cv_cumlogit_lcpo3[k] <- ifelse(lci3 < log(1.50) & uci3 > log(1.50), 1, 0)
  cv_cumlogit_lcpo4[k] <- ifelse(lci4 < log(1.50) & uci4 > log(1.50), 1, 0)
  cv_cumlogit_lcpo5[k] <- ifelse(lci5 < log(1.50) & uci5 > log(1.50), 1, 0)
  cv_cumlogit_lcpo6[k] <- ifelse(lci6 < log(1.50) & uci6 > log(1.50), 1, 0)
  cv_cumlogit_lcpo7[k] <- ifelse(lci7 < log(1.50) & uci7 > log(1.50), 1, 0)
  cv_cumlogit_lcpo8[k] <- ifelse(lci8 < log(1.50) & uci8 > log(1.50), 1, 0)
  cv_cumlogit_lcpo9[k] <- ifelse(lci9 < log(1.50) & uci9 > log(1.50), 1, 0)
  cv_cumlogit_lcpo10[k] <- ifelse(lci10 < log(1.50) & uci10 > log(1.50), 1, 0)
  
  # MSE 
  mse_cumlogit_lcpo1[k] <- (as.numeric(summarise_draws(phi1, "median")[1,2]) - log(1.50))^2
  mse_cumlogit_lcpo2[k] <- (as.numeric(summarise_draws(phi2, "median")[1,2]) - log(1.50))^2
  mse_cumlogit_lcpo3[k] <- (as.numeric(summarise_draws(phi3, "median")[1,2]) - log(1.50))^2
  mse_cumlogit_lcpo4[k] <- (as.numeric(summarise_draws(phi4, "median")[1,2]) - log(1.50))^2
  mse_cumlogit_lcpo5[k] <- (as.numeric(summarise_draws(phi5, "median")[1,2]) - log(1.50))^2
  mse_cumlogit_lcpo6[k] <- (as.numeric(summarise_draws(phi6, "median")[1,2]) - log(1.50))^2
  mse_cumlogit_lcpo7[k] <- (as.numeric(summarise_draws(phi7, "median")[1,2]) - log(1.50))^2
  mse_cumlogit_lcpo8[k] <- (as.numeric(summarise_draws(phi8, "median")[1,2]) - log(1.50))^2
  mse_cumlogit_lcpo9[k] <- (as.numeric(summarise_draws(phi9, "median")[1,2]) - log(1.50))^2
  mse_cumlogit_lcpo10[k] <- (as.numeric(summarise_draws(phi10, "median")[1,2]) - log(1.50))^2

  
  # MCSE 
  mcse_cumlogit_lcpo1[k] <- as.numeric(summarise_draws(phi1, "mcse_median")[1,2])
  mcse_cumlogit_lcpo2[k] <- as.numeric(summarise_draws(phi2, "mcse_median")[1,2])
  mcse_cumlogit_lcpo3[k] <- as.numeric(summarise_draws(phi3, "mcse_median")[1,2])
  mcse_cumlogit_lcpo4[k] <- as.numeric(summarise_draws(phi4, "mcse_median")[1,2])
  mcse_cumlogit_lcpo5[k] <- as.numeric(summarise_draws(phi5, "mcse_median")[1,2])
  mcse_cumlogit_lcpo6[k] <- as.numeric(summarise_draws(phi6, "mcse_median")[1,2])
  mcse_cumlogit_lcpo7[k] <- as.numeric(summarise_draws(phi7, "mcse_median")[1,2])
  mcse_cumlogit_lcpo8[k] <- as.numeric(summarise_draws(phi8, "mcse_median")[1,2])
  mcse_cumlogit_lcpo9[k] <- as.numeric(summarise_draws(phi9, "mcse_median")[1,2])
  mcse_cumlogit_lcpo10[k] <- as.numeric(summarise_draws(phi10, "mcse_median")[1,2])
 
  
  # R-hat 
  rhat_cumlogit_lcpo1[k] <- as.numeric(summarise_draws(phi1, "rhat")[1,2])
  rhat_cumlogit_lcpo2[k] <- as.numeric(summarise_draws(phi2, "rhat")[1,2])
  rhat_cumlogit_lcpo3[k] <- as.numeric(summarise_draws(phi3, "rhat")[1,2])
  rhat_cumlogit_lcpo4[k] <- as.numeric(summarise_draws(phi4, "rhat")[1,2])
  rhat_cumlogit_lcpo5[k] <- as.numeric(summarise_draws(phi5, "rhat")[1,2])
  rhat_cumlogit_lcpo6[k] <- as.numeric(summarise_draws(phi6, "rhat")[1,2])
  rhat_cumlogit_lcpo7[k] <- as.numeric(summarise_draws(phi7, "rhat")[1,2])
  rhat_cumlogit_lcpo8[k] <- as.numeric(summarise_draws(phi8, "rhat")[1,2])
  rhat_cumlogit_lcpo9[k] <- as.numeric(summarise_draws(phi9, "rhat")[1,2])
  rhat_cumlogit_lcpo10[k] <- as.numeric(summarise_draws(phi10, "rhat")[1,2])

  # Bulk ESS 
  bulkess_cumlogit_lcpo1[k] <- as.numeric(summarise_draws(phi1, "ess_bulk")[1,2])
  bulkess_cumlogit_lcpo2[k] <- as.numeric(summarise_draws(phi2, "ess_bulk")[1,2])
  bulkess_cumlogit_lcpo3[k] <- as.numeric(summarise_draws(phi3, "ess_bulk")[1,2])
  bulkess_cumlogit_lcpo4[k] <- as.numeric(summarise_draws(phi4, "ess_bulk")[1,2])
  bulkess_cumlogit_lcpo5[k] <- as.numeric(summarise_draws(phi5, "ess_bulk")[1,2])
  bulkess_cumlogit_lcpo6[k] <- as.numeric(summarise_draws(phi6, "ess_bulk")[1,2])
  bulkess_cumlogit_lcpo7[k] <- as.numeric(summarise_draws(phi7, "ess_bulk")[1,2])
  bulkess_cumlogit_lcpo8[k] <- as.numeric(summarise_draws(phi8, "ess_bulk")[1,2])
  bulkess_cumlogit_lcpo9[k] <- as.numeric(summarise_draws(phi9, "ess_bulk")[1,2])
  bulkess_cumlogit_lcpo10[k] <- as.numeric(summarise_draws(phi10, "ess_bulk")[1,2])

  
  # Tail ESS 
  tailess_cumlogit_lcpo1[k] <- as.numeric(summarise_draws(phi1, "ess_tail")[1,2])
  tailess_cumlogit_lcpo2[k] <- as.numeric(summarise_draws(phi2, "ess_tail")[1,2])
  tailess_cumlogit_lcpo3[k] <- as.numeric(summarise_draws(phi3, "ess_tail")[1,2])
  tailess_cumlogit_lcpo4[k] <- as.numeric(summarise_draws(phi4, "ess_tail")[1,2])
  tailess_cumlogit_lcpo5[k] <- as.numeric(summarise_draws(phi5, "ess_tail")[1,2])
  tailess_cumlogit_lcpo6[k] <- as.numeric(summarise_draws(phi6, "ess_tail")[1,2])
  tailess_cumlogit_lcpo7[k] <- as.numeric(summarise_draws(phi7, "ess_tail")[1,2])
  tailess_cumlogit_lcpo8[k] <- as.numeric(summarise_draws(phi8, "ess_tail")[1,2])
  tailess_cumlogit_lcpo9[k] <- as.numeric(summarise_draws(phi9, "ess_tail")[1,2])
  tailess_cumlogit_lcpo10[k] <- as.numeric(summarise_draws(phi10, "ess_tail")[1,2])

  
  # Number of divergent transitions 
  numdivergent_lcpo[k] <- get_num_divergent(stanGet(cppomodlin))
  
  
  # Number of missing values
  missing_lcpo[k] <- length(cppomodlin$na.action)
  
  
  ################### CONSTRAINED PPO MODEL ASSUMING DIVERGENCE ################
  
  cppomoddiv <- blrm(y~x,ppo = ~x, normcppo=F,
                     priorsdppo = 100,
                     conc = 1, seed=1234,iter=7500,chains=4,cppo=function(y) y==11,sampling.args = list(cores = 4,control=list(adapt_delta=0.9,max_treedepth=12)))
  
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
  
  phi7 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(8))
  phi7 <- subset_draws(phi7, c("phi"))
  
  phi8 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(9))
  phi8 <- subset_draws(phi8, c("phi"))
  
  phi9 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(10))
  phi9 <- subset_draws(phi9, c("phi"))
  
  phi10 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(11))
  phi10 <- subset_draws(phi10, c("phi"))
  
  
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_cumlogit_cpo1[k] <- as.numeric(summarise_draws(phi1, "median")[1,2]) - log(1.50)
  bias_cumlogit_cpo2[k] <- as.numeric(summarise_draws(phi2, "median")[1,2]) - log(1.50)
  bias_cumlogit_cpo3[k] <- as.numeric(summarise_draws(phi3, "median")[1,2]) - log(1.50)
  bias_cumlogit_cpo4[k] <- as.numeric(summarise_draws(phi4, "median")[1,2]) - log(1.50)
  bias_cumlogit_cpo5[k] <- as.numeric(summarise_draws(phi5, "median")[1,2]) - log(1.50)
  bias_cumlogit_cpo6[k] <- as.numeric(summarise_draws(phi6, "median")[1,2]) - log(1.50)
  bias_cumlogit_cpo7[k] <- as.numeric(summarise_draws(phi7, "median")[1,2]) - log(1.50)
  bias_cumlogit_cpo8[k] <- as.numeric(summarise_draws(phi8, "median")[1,2]) - log(1.50)
  bias_cumlogit_cpo9[k] <- as.numeric(summarise_draws(phi9, "median")[1,2]) - log(1.50)
  bias_cumlogit_cpo10[k] <- as.numeric(summarise_draws(phi10, "median")[1,2]) - log(1.50)

  
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
  
  lci7 <- as.numeric(summarise_draws(phi7, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci7 <- as.numeric(summarise_draws(phi7, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  lci8 <- as.numeric(summarise_draws(phi8, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci8 <- as.numeric(summarise_draws(phi8, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  lci9 <- as.numeric(summarise_draws(phi9, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci9 <- as.numeric(summarise_draws(phi9, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  lci10 <- as.numeric(summarise_draws(phi10, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci10 <- as.numeric(summarise_draws(phi10, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  
  cv_cumlogit_cpo1[k] <- ifelse(lci1 < log(1.50) & uci1 > log(1.50), 1, 0)
  cv_cumlogit_cpo2[k] <- ifelse(lci2 < log(1.50) & uci2 > log(1.50), 1, 0)
  cv_cumlogit_cpo3[k] <- ifelse(lci3 < log(1.50) & uci3 > log(1.50), 1, 0)
  cv_cumlogit_cpo4[k] <- ifelse(lci4 < log(1.50) & uci4 > log(1.50), 1, 0)
  cv_cumlogit_cpo5[k] <- ifelse(lci5 < log(1.50) & uci5 > log(1.50), 1, 0)
  cv_cumlogit_cpo6[k] <- ifelse(lci6 < log(1.50) & uci6 > log(1.50), 1, 0)
  cv_cumlogit_cpo7[k] <- ifelse(lci7 < log(1.50) & uci7 > log(1.50), 1, 0)
  cv_cumlogit_cpo8[k] <- ifelse(lci8 < log(1.50) & uci8 > log(1.50), 1, 0)
  cv_cumlogit_cpo9[k] <- ifelse(lci9 < log(1.50) & uci9 > log(1.50), 1, 0)
  cv_cumlogit_cpo10[k] <- ifelse(lci10 < log(1.50) & uci10 > log(1.50), 1, 0)

  
  # MSE 
  mse_cumlogit_cpo1[k] <- (as.numeric(summarise_draws(phi1, "median")[1,2]) - log(1.50))^2
  mse_cumlogit_cpo2[k] <- (as.numeric(summarise_draws(phi2, "median")[1,2]) - log(1.50))^2
  mse_cumlogit_cpo3[k] <- (as.numeric(summarise_draws(phi3, "median")[1,2]) - log(1.50))^2
  mse_cumlogit_cpo4[k] <- (as.numeric(summarise_draws(phi4, "median")[1,2]) - log(1.50))^2
  mse_cumlogit_cpo5[k] <- (as.numeric(summarise_draws(phi5, "median")[1,2]) - log(1.50))^2
  mse_cumlogit_cpo6[k] <- (as.numeric(summarise_draws(phi6, "median")[1,2]) - log(1.50))^2
  mse_cumlogit_cpo7[k] <- (as.numeric(summarise_draws(phi7, "median")[1,2]) - log(1.50))^2
  mse_cumlogit_cpo8[k] <- (as.numeric(summarise_draws(phi8, "median")[1,2]) - log(1.50))^2
  mse_cumlogit_cpo9[k] <- (as.numeric(summarise_draws(phi9, "median")[1,2]) - log(1.50))^2
  mse_cumlogit_cpo10[k] <- (as.numeric(summarise_draws(phi10, "median")[1,2]) - log(1.50))^2
  
  
  # MCSE 
  mcse_cumlogit_cpo1[k] <- as.numeric(summarise_draws(phi1, "mcse_median")[1,2])
  mcse_cumlogit_cpo2[k] <- as.numeric(summarise_draws(phi2, "mcse_median")[1,2])
  mcse_cumlogit_cpo3[k] <- as.numeric(summarise_draws(phi3, "mcse_median")[1,2])
  mcse_cumlogit_cpo4[k] <- as.numeric(summarise_draws(phi4, "mcse_median")[1,2])
  mcse_cumlogit_cpo5[k] <- as.numeric(summarise_draws(phi5, "mcse_median")[1,2])
  mcse_cumlogit_cpo6[k] <- as.numeric(summarise_draws(phi6, "mcse_median")[1,2])
  mcse_cumlogit_cpo7[k] <- as.numeric(summarise_draws(phi7, "mcse_median")[1,2])
  mcse_cumlogit_cpo8[k] <- as.numeric(summarise_draws(phi8, "mcse_median")[1,2])
  mcse_cumlogit_cpo9[k] <- as.numeric(summarise_draws(phi9, "mcse_median")[1,2])
  mcse_cumlogit_cpo10[k] <- as.numeric(summarise_draws(phi10, "mcse_median")[1,2])

  
  # R-hat 
  rhat_cumlogit_cpo1[k] <- as.numeric(summarise_draws(phi1, "rhat")[1,2])
  rhat_cumlogit_cpo2[k] <- as.numeric(summarise_draws(phi2, "rhat")[1,2])
  rhat_cumlogit_cpo3[k] <- as.numeric(summarise_draws(phi3, "rhat")[1,2])
  rhat_cumlogit_cpo4[k] <- as.numeric(summarise_draws(phi4, "rhat")[1,2])
  rhat_cumlogit_cpo5[k] <- as.numeric(summarise_draws(phi5, "rhat")[1,2])
  rhat_cumlogit_cpo6[k] <- as.numeric(summarise_draws(phi6, "rhat")[1,2])
  rhat_cumlogit_cpo7[k] <- as.numeric(summarise_draws(phi7, "rhat")[1,2])
  rhat_cumlogit_cpo8[k] <- as.numeric(summarise_draws(phi8, "rhat")[1,2])
  rhat_cumlogit_cpo9[k] <- as.numeric(summarise_draws(phi9, "rhat")[1,2])
  rhat_cumlogit_cpo10[k] <- as.numeric(summarise_draws(phi10, "rhat")[1,2])

  
  # Bulk ESS 
  bulkess_cumlogit_cpo1[k] <- as.numeric(summarise_draws(phi1, "ess_bulk")[1,2])
  bulkess_cumlogit_cpo2[k] <- as.numeric(summarise_draws(phi2, "ess_bulk")[1,2])
  bulkess_cumlogit_cpo3[k] <- as.numeric(summarise_draws(phi3, "ess_bulk")[1,2])
  bulkess_cumlogit_cpo4[k] <- as.numeric(summarise_draws(phi4, "ess_bulk")[1,2])
  bulkess_cumlogit_cpo5[k] <- as.numeric(summarise_draws(phi5, "ess_bulk")[1,2])
  bulkess_cumlogit_cpo6[k] <- as.numeric(summarise_draws(phi6, "ess_bulk")[1,2])
  bulkess_cumlogit_cpo7[k] <- as.numeric(summarise_draws(phi7, "ess_bulk")[1,2])
  bulkess_cumlogit_cpo8[k] <- as.numeric(summarise_draws(phi8, "ess_bulk")[1,2])
  bulkess_cumlogit_cpo9[k] <- as.numeric(summarise_draws(phi9, "ess_bulk")[1,2])
  bulkess_cumlogit_cpo10[k] <- as.numeric(summarise_draws(phi10, "ess_bulk")[1,2])

  
  # Tail ESS 
  tailess_cumlogit_cpo1[k] <- as.numeric(summarise_draws(phi1, "ess_tail")[1,2])
  tailess_cumlogit_cpo2[k] <- as.numeric(summarise_draws(phi2, "ess_tail")[1,2])
  tailess_cumlogit_cpo3[k] <- as.numeric(summarise_draws(phi3, "ess_tail")[1,2])
  tailess_cumlogit_cpo4[k] <- as.numeric(summarise_draws(phi4, "ess_tail")[1,2])
  tailess_cumlogit_cpo5[k] <- as.numeric(summarise_draws(phi5, "ess_tail")[1,2])
  tailess_cumlogit_cpo6[k] <- as.numeric(summarise_draws(phi6, "ess_tail")[1,2])
  tailess_cumlogit_cpo7[k] <- as.numeric(summarise_draws(phi7, "ess_tail")[1,2])
  tailess_cumlogit_cpo8[k] <- as.numeric(summarise_draws(phi8, "ess_tail")[1,2])
  tailess_cumlogit_cpo9[k] <- as.numeric(summarise_draws(phi9, "ess_tail")[1,2])
  tailess_cumlogit_cpo10[k] <- as.numeric(summarise_draws(phi10, "ess_tail")[1,2])

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
  bias_cumlogit_lr1[k] <- as.numeric(summarise_draws(df, "median")[2,2]) - log(1.50)
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  cv_cumlogit_lr1[k] <- ifelse(lci < log(1.50) & uci > log(1.50), 1, 0)
  
  # MSE 
  mse_cumlogit_lr1[k] <- (as.numeric(summarise_draws(df, "median")[2,2]) - log(1.50))^2

  
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
  bias_cumlogit_lr2[k] <- as.numeric(summarise_draws(df, "median")[2,2]) - log(1.50)
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  cv_cumlogit_lr2[k] <- ifelse(lci < log(1.50) & uci > log(1.50), 1, 0)
  
  # MSE 
  mse_cumlogit_lr2[k] <- (as.numeric(summarise_draws(df, "median")[2,2]) - log(1.50))^2
  
  
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
  bias_cumlogit_lr3[k] <- as.numeric(summarise_draws(df, "median")[2,2]) - log(1.50)
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  cv_cumlogit_lr3[k] <- ifelse(lci < log(1.50) & uci > log(1.50), 1, 0)
  
  # MSE 
  mse_cumlogit_lr3[k] <- (as.numeric(summarise_draws(df, "median")[2,2]) - log(1.50))^2
  
  
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
  bias_cumlogit_lr4[k] <- as.numeric(summarise_draws(df, "median")[2,2]) - log(1.50)
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  cv_cumlogit_lr4[k] <- ifelse(lci < log(1.50) & uci > log(1.50), 1, 0)
  
  # MSE 
  mse_cumlogit_lr4[k] <- (as.numeric(summarise_draws(df, "median")[2,2]) - log(1.50))^2
  
  
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
  bias_cumlogit_lr5[k] <- as.numeric(summarise_draws(df, "median")[2,2]) - log(1.50)
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  cv_cumlogit_lr5[k] <- ifelse(lci < log(1.50) & uci > log(1.50), 1, 0)
  
  # MSE 
  mse_cumlogit_lr5[k] <- (as.numeric(summarise_draws(df, "median")[2,2]) - log(1.50))^2
  
  
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
  bias_cumlogit_lr6[k] <- as.numeric(summarise_draws(df, "median")[2,2]) - log(1.50)
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  cv_cumlogit_lr6[k] <- ifelse(lci < log(1.50) & uci > log(1.50), 1, 0)
  
  # MSE 
  mse_cumlogit_lr6[k] <- (as.numeric(summarise_draws(df, "median")[2,2]) - log(1.50))^2
  
  
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
  
  ################### LOGISTIC MODEL - SEVENTH CUMULATIVE LOGIT  ################
  logmod7 <- blrm(y7~x1, 
                  pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                 contrast=expression(c2-c1), sd=100),
                  conc = 1, seed=1234,iter=7500,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(logmod7$draws)
  
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_cumlogit_lr7[k] <- as.numeric(summarise_draws(df, "median")[2,2]) - log(1.50)
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  cv_cumlogit_lr7[k] <- ifelse(lci < log(1.50) & uci > log(1.50), 1, 0)
  
  # MSE 
  mse_cumlogit_lr7[k] <- (as.numeric(summarise_draws(df, "median")[2,2]) - log(1.50))^2
  
  
  # MCSE 
  mcse_cumlogit_lr7[k] <- as.numeric(summarise_draws(df, "mcse_median")[2,2])
  
  
  # R-hat 
  rhat_cumlogit_lr7[k] <- as.numeric(summarise_draws(df, "rhat")[2,2])
  
  
  # Bulk ESS 
  bulkess_cumlogit_lr7[k] <- as.numeric(summarise_draws(df, "ess_bulk")[2,2])
  
  
  # Tail ESS 
  tailess_cumlogit_lr7[k] <- as.numeric(summarise_draws(df, "ess_tail")[2,2])
  
  
  # Number of divergent transitions 
  numdivergent_lr7[k] <- get_num_divergent(stanGet(logmod7))
  
  
  # Number of missing values
  missing_lr7[k] <- length(logmod7$na.action)
  
  ################### LOGISTIC MODEL - EIGHTH CUMULATIVE LOGIT  ################
  logmod8 <- blrm(y8~x1, 
                  pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                 contrast=expression(c2-c1), sd=100),
                  conc = 1, seed=1234,iter=7500,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(logmod8$draws)
  
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_cumlogit_lr8[k] <- as.numeric(summarise_draws(df, "median")[2,2]) - log(1.50)
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  cv_cumlogit_lr8[k] <- ifelse(lci < log(1.50) & uci > log(1.50), 1, 0)
  
  # MSE 
  mse_cumlogit_lr8[k] <- (as.numeric(summarise_draws(df, "median")[2,2]) - log(1.50))^2
  
  
  # MCSE 
  mcse_cumlogit_lr8[k] <- as.numeric(summarise_draws(df, "mcse_median")[2,2])
  
  
  # R-hat 
  rhat_cumlogit_lr8[k] <- as.numeric(summarise_draws(df, "rhat")[2,2])
  
  
  # Bulk ESS 
  bulkess_cumlogit_lr8[k] <- as.numeric(summarise_draws(df, "ess_bulk")[2,2])
  
  
  # Tail ESS 
  tailess_cumlogit_lr8[k] <- as.numeric(summarise_draws(df, "ess_tail")[2,2])
  
  
  # Number of divergent transitions 
  numdivergent_lr8[k] <- get_num_divergent(stanGet(logmod8))
  
  
  # Number of missing values
  missing_lr8[k] <- length(logmod8$na.action)
  
  ################### LOGISTIC MODEL - NINTH CUMULATIVE LOGIT  ################
  logmod9 <- blrm(y9~x1, 
                  pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                 contrast=expression(c2-c1), sd=100),
                  conc = 1, seed=1234,iter=7500,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(logmod9$draws)
  
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_cumlogit_lr9[k] <- as.numeric(summarise_draws(df, "median")[2,2]) - log(1.50)
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  cv_cumlogit_lr9[k] <- ifelse(lci < log(1.50) & uci > log(1.50), 1, 0)
  
  # MSE 
  mse_cumlogit_lr9[k] <- (as.numeric(summarise_draws(df, "median")[2,2]) - log(1.50))^2
  
  
  # MCSE 
  mcse_cumlogit_lr9[k] <- as.numeric(summarise_draws(df, "mcse_median")[2,2])
  
  
  # R-hat 
  rhat_cumlogit_lr9[k] <- as.numeric(summarise_draws(df, "rhat")[2,2])
  
  
  # Bulk ESS 
  bulkess_cumlogit_lr9[k] <- as.numeric(summarise_draws(df, "ess_bulk")[2,2])
  
  
  # Tail ESS 
  tailess_cumlogit_lr9[k] <- as.numeric(summarise_draws(df, "ess_tail")[2,2])
  
  
  # Number of divergent transitions 
  numdivergent_lr9[k] <- get_num_divergent(stanGet(logmod9))
  
  
  # Number of missing values
  missing_lr9[k] <- length(logmod9$na.action)
  
  ################### LOGISTIC MODEL - TENTH CUMULATIVE LOGIT  ################
  logmod10 <- blrm(y10~x1, 
                  pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                 contrast=expression(c2-c1), sd=100),
                  conc = 1, seed=1234,iter=7500,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(logmod10$draws)
  
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_cumlogit_lr10[k] <- as.numeric(summarise_draws(df, "median")[2,2]) - log(1.50)
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  cv_cumlogit_lr10[k] <- ifelse(lci < log(1.50) & uci > log(1.50), 1, 0)
  
  # MSE 
  mse_cumlogit_lr10[k] <- (as.numeric(summarise_draws(df, "median")[2,2]) - log(1.50))^2
  
  
  # MCSE 
  mcse_cumlogit_lr10[k] <- as.numeric(summarise_draws(df, "mcse_median")[2,2])
  
  
  # R-hat 
  rhat_cumlogit_lr10[k] <- as.numeric(summarise_draws(df, "rhat")[2,2])
  
  
  # Bulk ESS 
  bulkess_cumlogit_lr10[k] <- as.numeric(summarise_draws(df, "ess_bulk")[2,2])
  
  
  # Tail ESS 
  tailess_cumlogit_lr10[k] <- as.numeric(summarise_draws(df, "ess_tail")[2,2])
  
  
  # Number of divergent transitions 
  numdivergent_lr10[k] <- get_num_divergent(stanGet(logmod10))
  
  
  # Number of missing values
  missing_lr10[k] <- length(logmod10$na.action)
}


## Format data for analysis 
data <- data.frame(bias_cumlogit_cpo1 = bias_cumlogit_cpo1, bias_cumlogit_cpo2 = bias_cumlogit_cpo2,
                   bias_cumlogit_cpo3 = bias_cumlogit_cpo3, bias_cumlogit_cpo4 = bias_cumlogit_cpo4,
                   bias_cumlogit_cpo5 = bias_cumlogit_cpo5, bias_cumlogit_cpo6 = bias_cumlogit_cpo6,
                   bias_cumlogit_cpo7 = bias_cumlogit_cpo7, bias_cumlogit_cpo8 = bias_cumlogit_cpo8,
                   bias_cumlogit_cpo9 = bias_cumlogit_cpo9, bias_cumlogit_cpo10 = bias_cumlogit_cpo10,
                   bias_cumlogit_lcpo1 = bias_cumlogit_lcpo1,bias_cumlogit_lcpo2 = bias_cumlogit_lcpo2,
                   bias_cumlogit_lcpo3 = bias_cumlogit_lcpo3,bias_cumlogit_lcpo4 = bias_cumlogit_lcpo4,
                   bias_cumlogit_lcpo5 = bias_cumlogit_lcpo5,bias_cumlogit_lcpo6 = bias_cumlogit_lcpo6,
                   bias_cumlogit_lcpo7 = bias_cumlogit_lcpo7,bias_cumlogit_lcpo8 = bias_cumlogit_lcpo8,
                   bias_cumlogit_lcpo9 = bias_cumlogit_lcpo9,bias_cumlogit_lcpo10 = bias_cumlogit_lcpo10,
                   bias_cumlogit_lr1 = bias_cumlogit_lr1,bias_cumlogit_lr2 = bias_cumlogit_lr2,
                   bias_cumlogit_lr3 = bias_cumlogit_lr3,bias_cumlogit_lr4 = bias_cumlogit_lr4,
                   bias_cumlogit_lr5 = bias_cumlogit_lr5,bias_cumlogit_lr6 = bias_cumlogit_lr6,
                   bias_cumlogit_lr7 = bias_cumlogit_lr7,bias_cumlogit_lr8 = bias_cumlogit_lr8,
                   bias_cumlogit_lr9 = bias_cumlogit_lr9,bias_cumlogit_lr10 = bias_cumlogit_lr10,
                   bias_cumlogit_po1 = bias_cumlogit_po1,bias_cumlogit_po2 = bias_cumlogit_po2,
                   bias_cumlogit_po3 = bias_cumlogit_po3,bias_cumlogit_po4 = bias_cumlogit_po4,
                   bias_cumlogit_po5 = bias_cumlogit_po5,bias_cumlogit_po6 = bias_cumlogit_po6,
                   bias_cumlogit_po7 = bias_cumlogit_po7,bias_cumlogit_po8 = bias_cumlogit_po8,
                   bias_cumlogit_po9 = bias_cumlogit_po9,bias_cumlogit_po10 = bias_cumlogit_po10,
                   bias_cumlogit_ppo1 = bias_cumlogit_ppo1,bias_cumlogit_ppo2 = bias_cumlogit_ppo2,
                   bias_cumlogit_ppo3 = bias_cumlogit_ppo3,bias_cumlogit_ppo4 = bias_cumlogit_ppo4,
                   bias_cumlogit_ppo5 = bias_cumlogit_ppo5,bias_cumlogit_ppo6 = bias_cumlogit_ppo6,
                   bias_cumlogit_ppo7 = bias_cumlogit_ppo7,bias_cumlogit_ppo8 = bias_cumlogit_ppo8,
                   bias_cumlogit_ppo9 = bias_cumlogit_ppo9,bias_cumlogit_ppo10 = bias_cumlogit_ppo10,
                   cv_cumlogit_cpo1 = cv_cumlogit_cpo1, cv_cumlogit_cpo2 = cv_cumlogit_cpo2,
                   cv_cumlogit_cpo3 = cv_cumlogit_cpo3, cv_cumlogit_cpo4 = cv_cumlogit_cpo4,
                   cv_cumlogit_cpo5 = cv_cumlogit_cpo5, cv_cumlogit_cpo6 = cv_cumlogit_cpo6,
                   cv_cumlogit_cpo7 = cv_cumlogit_cpo7, cv_cumlogit_cpo8 = cv_cumlogit_cpo8,
                   cv_cumlogit_cpo9 = cv_cumlogit_cpo9, cv_cumlogit_cpo10 = cv_cumlogit_cpo10,
                   cv_cumlogit_lcpo1 = cv_cumlogit_lcpo1,cv_cumlogit_lcpo2 = cv_cumlogit_lcpo2,
                   cv_cumlogit_lcpo3 = cv_cumlogit_lcpo3,cv_cumlogit_lcpo4 = cv_cumlogit_lcpo4,
                   cv_cumlogit_lcpo5 = cv_cumlogit_lcpo5,cv_cumlogit_lcpo6 = cv_cumlogit_lcpo6,
                   cv_cumlogit_lcpo7 = cv_cumlogit_lcpo7,cv_cumlogit_lcpo8 = cv_cumlogit_lcpo8,
                   cv_cumlogit_lcpo9 = cv_cumlogit_lcpo9,cv_cumlogit_lcpo10 = cv_cumlogit_lcpo10,
                   cv_cumlogit_lr1 = cv_cumlogit_lr1,cv_cumlogit_lr2 = cv_cumlogit_lr2,
                   cv_cumlogit_lr3 = cv_cumlogit_lr3,cv_cumlogit_lr4 = cv_cumlogit_lr4,
                   cv_cumlogit_lr5 = cv_cumlogit_lr5,cv_cumlogit_lr6 = cv_cumlogit_lr6,
                   cv_cumlogit_lr7 = cv_cumlogit_lr7,cv_cumlogit_lr8 = cv_cumlogit_lr8,
                   cv_cumlogit_lr9 = cv_cumlogit_lr9,cv_cumlogit_lr10 = cv_cumlogit_lr10,
                   cv_cumlogit_po1 = cv_cumlogit_po1,cv_cumlogit_po2 = cv_cumlogit_po2,
                   cv_cumlogit_po3 = cv_cumlogit_po3,cv_cumlogit_po4 = cv_cumlogit_po4,
                   cv_cumlogit_po5 = cv_cumlogit_po5,cv_cumlogit_po6 = cv_cumlogit_po6,
                   cv_cumlogit_po7 = cv_cumlogit_po7,cv_cumlogit_po8 = cv_cumlogit_po8,
                   cv_cumlogit_po9 = cv_cumlogit_po9,cv_cumlogit_po10 = cv_cumlogit_po10,
                   cv_cumlogit_ppo1 = cv_cumlogit_ppo1,cv_cumlogit_ppo2 = cv_cumlogit_ppo2,
                   cv_cumlogit_ppo3 = cv_cumlogit_ppo3,cv_cumlogit_ppo4 = cv_cumlogit_ppo4,
                   cv_cumlogit_ppo5 = cv_cumlogit_ppo5,cv_cumlogit_ppo6 = cv_cumlogit_ppo6,
                   cv_cumlogit_ppo7 = cv_cumlogit_ppo7,cv_cumlogit_ppo8 = cv_cumlogit_ppo8,
                   cv_cumlogit_ppo9 = cv_cumlogit_ppo9,cv_cumlogit_ppo10 = cv_cumlogit_ppo10,
                   mcse_cumlogit_cpo1 = mcse_cumlogit_cpo1, mcse_cumlogit_cpo2 = mcse_cumlogit_cpo2,
                   mcse_cumlogit_cpo3 = mcse_cumlogit_cpo3, mcse_cumlogit_cpo4 = mcse_cumlogit_cpo4,
                   mcse_cumlogit_cpo5 = mcse_cumlogit_cpo5, mcse_cumlogit_cpo6 = mcse_cumlogit_cpo6,
                   mcse_cumlogit_cpo7 = mcse_cumlogit_cpo7, mcse_cumlogit_cpo8 = mcse_cumlogit_cpo8,
                   mcse_cumlogit_cpo9 = mcse_cumlogit_cpo9, mcse_cumlogit_cpo10 = mcse_cumlogit_cpo10,
                   mcse_cumlogit_lcpo1 = mcse_cumlogit_lcpo1,mcse_cumlogit_lcpo2 = mcse_cumlogit_lcpo2,
                   mcse_cumlogit_lcpo3 = mcse_cumlogit_lcpo3,mcse_cumlogit_lcpo4 = mcse_cumlogit_lcpo4,
                   mcse_cumlogit_lcpo5 = mcse_cumlogit_lcpo5,mcse_cumlogit_lcpo6 = mcse_cumlogit_lcpo6,
                   mcse_cumlogit_lcpo7 = mcse_cumlogit_lcpo7,mcse_cumlogit_lcpo8 = mcse_cumlogit_lcpo8,
                   mcse_cumlogit_lcpo9 = mcse_cumlogit_lcpo9,mcse_cumlogit_lcpo10 = mcse_cumlogit_lcpo10,
                   mcse_cumlogit_lr1 = mcse_cumlogit_lr1,mcse_cumlogit_lr2 = mcse_cumlogit_lr2,
                   mcse_cumlogit_lr3 = mcse_cumlogit_lr3,mcse_cumlogit_lr4 = mcse_cumlogit_lr4,
                   mcse_cumlogit_lr5 = mcse_cumlogit_lr5,mcse_cumlogit_lr6 = mcse_cumlogit_lr6,
                   mcse_cumlogit_lr7 = mcse_cumlogit_lr7,mcse_cumlogit_lr8 = mcse_cumlogit_lr8,
                   mcse_cumlogit_lr9 = mcse_cumlogit_lr9,mcse_cumlogit_lr10 = mcse_cumlogit_lr10,
                   mcse_cumlogit_po1 = mcse_cumlogit_po1,mcse_cumlogit_po2 = mcse_cumlogit_po2,
                   mcse_cumlogit_po3 = mcse_cumlogit_po3,mcse_cumlogit_po4 = mcse_cumlogit_po4,
                   mcse_cumlogit_po5 = mcse_cumlogit_po5,mcse_cumlogit_po6 = mcse_cumlogit_po6,
                   mcse_cumlogit_po7 = mcse_cumlogit_po7,mcse_cumlogit_po8 = mcse_cumlogit_po8,
                   mcse_cumlogit_po9 = mcse_cumlogit_po9,mcse_cumlogit_po10 = mcse_cumlogit_po10,
                   mcse_cumlogit_ppo1 = mcse_cumlogit_ppo1,mcse_cumlogit_ppo2 = mcse_cumlogit_ppo2,
                   mcse_cumlogit_ppo3 = mcse_cumlogit_ppo3,mcse_cumlogit_ppo4 = mcse_cumlogit_ppo4,
                   mcse_cumlogit_ppo5 = mcse_cumlogit_ppo5,mcse_cumlogit_ppo6 = mcse_cumlogit_ppo6,
                   mcse_cumlogit_ppo7 = mcse_cumlogit_ppo7,mcse_cumlogit_ppo8 = mcse_cumlogit_ppo8,
                   mcse_cumlogit_ppo9 = mcse_cumlogit_ppo9,mcse_cumlogit_ppo10 = mcse_cumlogit_ppo10,
                   bulkess_cumlogit_cpo1 = bulkess_cumlogit_cpo1, bulkess_cumlogit_cpo2 = bulkess_cumlogit_cpo2,
                   bulkess_cumlogit_cpo3 = bulkess_cumlogit_cpo3, bulkess_cumlogit_cpo4 = bulkess_cumlogit_cpo4,
                   bulkess_cumlogit_cpo5 = bulkess_cumlogit_cpo5, bulkess_cumlogit_cpo6 = bulkess_cumlogit_cpo6,
                   bulkess_cumlogit_cpo7 = bulkess_cumlogit_cpo7, bulkess_cumlogit_cpo8 = bulkess_cumlogit_cpo8,
                   bulkess_cumlogit_cpo9 = bulkess_cumlogit_cpo9, bulkess_cumlogit_cpo10 = bulkess_cumlogit_cpo10,
                   bulkess_cumlogit_lcpo1 = bulkess_cumlogit_lcpo1,bulkess_cumlogit_lcpo2 = bulkess_cumlogit_lcpo2,
                   bulkess_cumlogit_lcpo3 = bulkess_cumlogit_lcpo3,bulkess_cumlogit_lcpo4 = bulkess_cumlogit_lcpo4,
                   bulkess_cumlogit_lcpo5 = bulkess_cumlogit_lcpo5,bulkess_cumlogit_lcpo6 = bulkess_cumlogit_lcpo6,
                   bulkess_cumlogit_lcpo7 = bulkess_cumlogit_lcpo7,bulkess_cumlogit_lcpo8 = bulkess_cumlogit_lcpo8,
                   bulkess_cumlogit_lcpo9 = bulkess_cumlogit_lcpo9,bulkess_cumlogit_lcpo10 = bulkess_cumlogit_lcpo10,
                   bulkess_cumlogit_lr1 = bulkess_cumlogit_lr1,bulkess_cumlogit_lr2 = bulkess_cumlogit_lr2,
                   bulkess_cumlogit_lr3 = bulkess_cumlogit_lr3,bulkess_cumlogit_lr4 = bulkess_cumlogit_lr4,
                   bulkess_cumlogit_lr5 = bulkess_cumlogit_lr5,bulkess_cumlogit_lr6 = bulkess_cumlogit_lr6,
                   bulkess_cumlogit_lr7 = bulkess_cumlogit_lr7,bulkess_cumlogit_lr8 = bulkess_cumlogit_lr8,
                   bulkess_cumlogit_lr9 = bulkess_cumlogit_lr9,bulkess_cumlogit_lr10 = bulkess_cumlogit_lr10,
                   bulkess_cumlogit_po1 = bulkess_cumlogit_po1,bulkess_cumlogit_po2 = bulkess_cumlogit_po2,
                   bulkess_cumlogit_po3 = bulkess_cumlogit_po3,bulkess_cumlogit_po4 = bulkess_cumlogit_po4,
                   bulkess_cumlogit_po5 = bulkess_cumlogit_po5,bulkess_cumlogit_po6 = bulkess_cumlogit_po6,
                   bulkess_cumlogit_po7 = bulkess_cumlogit_po7,bulkess_cumlogit_po8 = bulkess_cumlogit_po8,
                   bulkess_cumlogit_po9 = bulkess_cumlogit_po9,bulkess_cumlogit_po10 = bulkess_cumlogit_po10,
                   bulkess_cumlogit_ppo1 = bulkess_cumlogit_ppo1,bulkess_cumlogit_ppo2 = bulkess_cumlogit_ppo2,
                   bulkess_cumlogit_ppo3 = bulkess_cumlogit_ppo3,bulkess_cumlogit_ppo4 = bulkess_cumlogit_ppo4,
                   bulkess_cumlogit_ppo5 = bulkess_cumlogit_ppo5,bulkess_cumlogit_ppo6 = bulkess_cumlogit_ppo6,
                   bulkess_cumlogit_ppo7 = bulkess_cumlogit_ppo7,bulkess_cumlogit_ppo8 = bulkess_cumlogit_ppo8,
                   bulkess_cumlogit_ppo9 = bulkess_cumlogit_ppo9,bulkess_cumlogit_ppo10 = bulkess_cumlogit_ppo10,
                   tailess_cumlogit_cpo1 = tailess_cumlogit_cpo1, tailess_cumlogit_cpo2 = tailess_cumlogit_cpo2,
                   tailess_cumlogit_cpo3 = tailess_cumlogit_cpo3, tailess_cumlogit_cpo4 = tailess_cumlogit_cpo4,
                   tailess_cumlogit_cpo5 = tailess_cumlogit_cpo5, tailess_cumlogit_cpo6 = tailess_cumlogit_cpo6,
                   tailess_cumlogit_cpo7 = tailess_cumlogit_cpo7, tailess_cumlogit_cpo8 = tailess_cumlogit_cpo8,
                   tailess_cumlogit_cpo9 = tailess_cumlogit_cpo9, tailess_cumlogit_cpo10 = tailess_cumlogit_cpo10,
                   tailess_cumlogit_lcpo1 = tailess_cumlogit_lcpo1,tailess_cumlogit_lcpo2 = tailess_cumlogit_lcpo2,
                   tailess_cumlogit_lcpo3 = tailess_cumlogit_lcpo3,tailess_cumlogit_lcpo4 = tailess_cumlogit_lcpo4,
                   tailess_cumlogit_lcpo5 = tailess_cumlogit_lcpo5,tailess_cumlogit_lcpo6 = tailess_cumlogit_lcpo6,
                   tailess_cumlogit_lcpo7 = tailess_cumlogit_lcpo7,tailess_cumlogit_lcpo8 = tailess_cumlogit_lcpo8,
                   tailess_cumlogit_lcpo9 = tailess_cumlogit_lcpo9,tailess_cumlogit_lcpo10 = tailess_cumlogit_lcpo10,
                   tailess_cumlogit_lr1 = tailess_cumlogit_lr1,tailess_cumlogit_lr2 = tailess_cumlogit_lr2,
                   tailess_cumlogit_lr3 = tailess_cumlogit_lr3,tailess_cumlogit_lr4 = tailess_cumlogit_lr4,
                   tailess_cumlogit_lr5 = tailess_cumlogit_lr5,tailess_cumlogit_lr6 = tailess_cumlogit_lr6,
                   tailess_cumlogit_lr7 = tailess_cumlogit_lr7,tailess_cumlogit_lr8 = tailess_cumlogit_lr8,
                   tailess_cumlogit_lr9 = tailess_cumlogit_lr9,tailess_cumlogit_lr10 = tailess_cumlogit_lr10,
                   tailess_cumlogit_po1 = tailess_cumlogit_po1,tailess_cumlogit_po2 = tailess_cumlogit_po2,
                   tailess_cumlogit_po3 = tailess_cumlogit_po3,tailess_cumlogit_po4 = tailess_cumlogit_po4,
                   tailess_cumlogit_po5 = tailess_cumlogit_po5,tailess_cumlogit_po6 = tailess_cumlogit_po6,
                   tailess_cumlogit_po7 = tailess_cumlogit_po7,tailess_cumlogit_po8 = tailess_cumlogit_po8,
                   tailess_cumlogit_po9 = tailess_cumlogit_po9,tailess_cumlogit_po10 = tailess_cumlogit_po10,
                   tailess_cumlogit_ppo1 = tailess_cumlogit_ppo1,tailess_cumlogit_ppo2 = tailess_cumlogit_ppo2,
                   tailess_cumlogit_ppo3 = tailess_cumlogit_ppo3,tailess_cumlogit_ppo4 = tailess_cumlogit_ppo4,
                   tailess_cumlogit_ppo5 = tailess_cumlogit_ppo5,tailess_cumlogit_ppo6 = tailess_cumlogit_ppo6,
                   tailess_cumlogit_ppo7 = tailess_cumlogit_ppo7,tailess_cumlogit_ppo8 = tailess_cumlogit_ppo8,
                   tailess_cumlogit_ppo9 = tailess_cumlogit_ppo9,tailess_cumlogit_ppo10 = tailess_cumlogit_ppo10,
                   mse_cumlogit_cpo1 = mse_cumlogit_cpo1, mse_cumlogit_cpo2 = mse_cumlogit_cpo2,
                   mse_cumlogit_cpo3 = mse_cumlogit_cpo3, mse_cumlogit_cpo4 = mse_cumlogit_cpo4,
                   mse_cumlogit_cpo5 = mse_cumlogit_cpo5, mse_cumlogit_cpo6 = mse_cumlogit_cpo6,
                   mse_cumlogit_cpo7 = mse_cumlogit_cpo7, mse_cumlogit_cpo8 = mse_cumlogit_cpo8,
                   mse_cumlogit_cpo9 = mse_cumlogit_cpo9, mse_cumlogit_cpo10 = mse_cumlogit_cpo10,
                   mse_cumlogit_lcpo1 = mse_cumlogit_lcpo1,mse_cumlogit_lcpo2 = mse_cumlogit_lcpo2,
                   mse_cumlogit_lcpo3 = mse_cumlogit_lcpo3,mse_cumlogit_lcpo4 = mse_cumlogit_lcpo4,
                   mse_cumlogit_lcpo5 = mse_cumlogit_lcpo5,mse_cumlogit_lcpo6 = mse_cumlogit_lcpo6,
                   mse_cumlogit_lcpo7 = mse_cumlogit_lcpo7,mse_cumlogit_lcpo8 = mse_cumlogit_lcpo8,
                   mse_cumlogit_lcpo9 = mse_cumlogit_lcpo9,mse_cumlogit_lcpo10 = mse_cumlogit_lcpo10,
                   mse_cumlogit_lr1 = mse_cumlogit_lr1,mse_cumlogit_lr2 = mse_cumlogit_lr2,
                   mse_cumlogit_lr3 = mse_cumlogit_lr3,mse_cumlogit_lr4 = mse_cumlogit_lr4,
                   mse_cumlogit_lr5 = mse_cumlogit_lr5,mse_cumlogit_lr6 = mse_cumlogit_lr6,
                   mse_cumlogit_lr7 = mse_cumlogit_lr7,mse_cumlogit_lr8 = mse_cumlogit_lr8,
                   mse_cumlogit_lr9 = mse_cumlogit_lr9,mse_cumlogit_lr10 = mse_cumlogit_lr10,
                   mse_cumlogit_po1 = mse_cumlogit_po1,mse_cumlogit_po2 = mse_cumlogit_po2,
                   mse_cumlogit_po3 = mse_cumlogit_po3,mse_cumlogit_po4 = mse_cumlogit_po4,
                   mse_cumlogit_po5 = mse_cumlogit_po5,mse_cumlogit_po6 = mse_cumlogit_po6,
                   mse_cumlogit_po7 = mse_cumlogit_po7,mse_cumlogit_po8 = mse_cumlogit_po8,
                   mse_cumlogit_po9 = mse_cumlogit_po9,mse_cumlogit_po10 = mse_cumlogit_po10,
                   mse_cumlogit_ppo1 = mse_cumlogit_ppo1,mse_cumlogit_ppo2 = mse_cumlogit_ppo2,
                   mse_cumlogit_ppo3 = mse_cumlogit_ppo3,mse_cumlogit_ppo4 = mse_cumlogit_ppo4,
                   mse_cumlogit_ppo5 = mse_cumlogit_ppo5,mse_cumlogit_ppo6 = mse_cumlogit_ppo6,
                   mse_cumlogit_ppo7 = mse_cumlogit_ppo7,mse_cumlogit_ppo8 = mse_cumlogit_ppo8,
                   mse_cumlogit_ppo9 = mse_cumlogit_ppo9,mse_cumlogit_ppo10 = mse_cumlogit_ppo10,
                   rhat_cumlogit_cpo1 = rhat_cumlogit_cpo1, rhat_cumlogit_cpo2 = rhat_cumlogit_cpo2,
                   rhat_cumlogit_cpo3 = rhat_cumlogit_cpo3, rhat_cumlogit_cpo4 = rhat_cumlogit_cpo4,
                   rhat_cumlogit_cpo5 = rhat_cumlogit_cpo5, rhat_cumlogit_cpo6 = rhat_cumlogit_cpo6,
                   rhat_cumlogit_cpo7 = rhat_cumlogit_cpo7, rhat_cumlogit_cpo8 = rhat_cumlogit_cpo8,
                   rhat_cumlogit_cpo9 = rhat_cumlogit_cpo9, rhat_cumlogit_cpo10 = rhat_cumlogit_cpo10,
                   rhat_cumlogit_lcpo1 = rhat_cumlogit_lcpo1,rhat_cumlogit_lcpo2 = rhat_cumlogit_lcpo2,
                   rhat_cumlogit_lcpo3 = rhat_cumlogit_lcpo3,rhat_cumlogit_lcpo4 = rhat_cumlogit_lcpo4,
                   rhat_cumlogit_lcpo5 = rhat_cumlogit_lcpo5,rhat_cumlogit_lcpo6 = rhat_cumlogit_lcpo6,
                   rhat_cumlogit_lcpo7 = rhat_cumlogit_lcpo7,rhat_cumlogit_lcpo8 = rhat_cumlogit_lcpo8,
                   rhat_cumlogit_lcpo9 = rhat_cumlogit_lcpo9,rhat_cumlogit_lcpo10 = rhat_cumlogit_lcpo10,
                   rhat_cumlogit_lr1 = rhat_cumlogit_lr1,rhat_cumlogit_lr2 = rhat_cumlogit_lr2,
                   rhat_cumlogit_lr3 = rhat_cumlogit_lr3,rhat_cumlogit_lr4 = rhat_cumlogit_lr4,
                   rhat_cumlogit_lr5 = rhat_cumlogit_lr5,rhat_cumlogit_lr6 = rhat_cumlogit_lr6,
                   rhat_cumlogit_lr7 = rhat_cumlogit_lr7,rhat_cumlogit_lr8 = rhat_cumlogit_lr8,
                   rhat_cumlogit_lr9 = rhat_cumlogit_lr9,rhat_cumlogit_lr10 = rhat_cumlogit_lr10,
                   rhat_cumlogit_po1 = rhat_cumlogit_po1,rhat_cumlogit_po2 = rhat_cumlogit_po2,
                   rhat_cumlogit_po3 = rhat_cumlogit_po3,rhat_cumlogit_po4 = rhat_cumlogit_po4,
                   rhat_cumlogit_po5 = rhat_cumlogit_po5,rhat_cumlogit_po6 = rhat_cumlogit_po6,
                   rhat_cumlogit_po7 = rhat_cumlogit_po7,rhat_cumlogit_po8 = rhat_cumlogit_po8,
                   rhat_cumlogit_po9 = rhat_cumlogit_po9,rhat_cumlogit_po10 = rhat_cumlogit_po10,
                   rhat_cumlogit_ppo1 = rhat_cumlogit_ppo1,rhat_cumlogit_ppo2 = rhat_cumlogit_ppo2,
                   rhat_cumlogit_ppo3 = rhat_cumlogit_ppo3,rhat_cumlogit_ppo4 = rhat_cumlogit_ppo4,
                   rhat_cumlogit_ppo5 = rhat_cumlogit_ppo5,rhat_cumlogit_ppo6 = rhat_cumlogit_ppo6,
                   rhat_cumlogit_ppo7 = rhat_cumlogit_ppo7,rhat_cumlogit_ppo8 = rhat_cumlogit_ppo8,
                   rhat_cumlogit_ppo9 = rhat_cumlogit_ppo9,rhat_cumlogit_ppo10 = rhat_cumlogit_ppo10,
                   missing_cpo = missing_cpo, missing_lcpo = missing_lcpo,
                   missing_lr1 = missing_lr1,missing_lr2 = missing_lr2,
                   missing_lr3 = missing_lr3,missing_lr4 = missing_lr4,
                   missing_lr5 = missing_lr5,missing_lr6 = missing_lr6,
                   missing_lr7 = missing_lr7,missing_lr8 = missing_lr8,
                   missing_lr9 = missing_lr9,missing_lr10 = missing_lr10,
                   missing_po = missing_po,missing_ppo = missing_ppo,
                   numdivergent_cpo = numdivergent_cpo, numdivergent_lcpo = numdivergent_lcpo,
                   numdivergent_lr1 = numdivergent_lr1,numdivergent_lr2 = numdivergent_lr2,
                   numdivergent_lr3 = numdivergent_lr3,numdivergent_lr4 = numdivergent_lr4,
                   numdivergent_lr5 = numdivergent_lr5,numdivergent_lr6 = numdivergent_lr6,
                   numdivergent_lr7 = numdivergent_lr7,numdivergent_lr8 = numdivergent_lr8,
                   numdivergent_lr9 = numdivergent_lr9,numdivergent_lr10 = numdivergent_lr10,
                   numdivergent_po = numdivergent_po,numdivergent_ppo = numdivergent_ppo
)  

data <- data[s1[datnum]:s2[datnum],]

save(data, file = paste0("smalln_skew_11cat_randompo_modeff_slight",datnum,".Rdata"))

end <- Sys.time()
start-end


