## Set working directory 
setwd("/group/cebu1/BACKUP/Chris/Project_2/Main_Simulations/Large_n/Skewed/Three_categories/Linear")


## Main simulation scenario 

start <- Sys.time()
rm(list=ls())

options(mc.cores = 8)
## Load in the appropriate packages
#library("rstanarm")
library("rstan")
library('posterior')
library("rmsb")
library("truncnorm")

args = commandArgs(trailingOnly = TRUE)
datnum <- as.numeric(args[1])

# Load the 1000 datasets
load("largen_skew_3cat_linear_dataset.Rdata")
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
states <- c("A", "B", "C")

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
missing_po <- missing_ppo <- missing_cpo <- missing_lcpo <- missing_lr1 <- missing_lr2 <- length(nsim) # to store number of missing values in each model 


## Number of divergent transitions 
numdivergent_po <- numdivergent_ppo <- numdivergent_cpo <- numdivergent_lcpo <- numdivergent_lr1 <- numdivergent_lr2 <- length(nsim) # to store number of divergent transitions in each model 


# To specify prior SDs for contrasts using blrm for PO and logistic models 
. <- function(...) list(...)

for (k in s1[datnum]:s2[datnum]){
  x <- big_data$x[big_data$k == k]
  y <- big_data$y[big_data$k == k]
  x1 <- big_data$x1[big_data$k == k]
  y1 <- big_data$y1[big_data$k == k]
  y2 <- big_data$y2[big_data$k == k]
  
  ##################### Proportional odds model ##################
  pomod <- blrm(y~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                  contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=7500,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(pomod$draws)
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_cumlogit_po1[k] <- as.numeric(summarise_draws(df, "median")[3,2]) - log(0.8)
  bias_cumlogit_po2[k] <- as.numeric(summarise_draws(df, "median")[3,2]) - (log(0.8) + 0.06)
  
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[3,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[3,3])

  cv_cumlogit_po1[k] <- ifelse(lci < log(0.8) & uci > log(0.8), 1, 0)
  cv_cumlogit_po2[k] <- ifelse(lci < (log(0.8) + 0.06) & uci > (log(0.8) + 0.06), 1, 0)
  
  
  # MSE 
  mse_cumlogit_po1[k] <- (as.numeric(summarise_draws(df, "median")[3,2]) - log(0.8))^2
  mse_cumlogit_po2[k] <- (as.numeric(summarise_draws(df, "median")[3,2]) - (log(0.8) + 0.06))^2
  
  
  # MCSE 
  mcse_cumlogit_po1[k] <- as.numeric(summarise_draws(df, "mcse_median")[3,2])
  mcse_cumlogit_po2[k] <- as.numeric(summarise_draws(df, "mcse_median")[3,2])
  
  
  # R-hat 
  rhat_cumlogit_po1[k] <- as.numeric(summarise_draws(df, "rhat")[3,2])
  rhat_cumlogit_po2[k] <- as.numeric(summarise_draws(df, "rhat")[3,2])
  
  
  # Bulk ESS 
  bulkess_cumlogit_po1[k] <- as.numeric(summarise_draws(df, "ess_bulk")[3,2])
  bulkess_cumlogit_po2[k] <- as.numeric(summarise_draws(df, "ess_bulk")[3,2])
  
  
  # Tail ESS 
  tailess_cumlogit_po1[k] <- as.numeric(summarise_draws(df, "ess_tail")[3,2])
  tailess_cumlogit_po2[k] <- as.numeric(summarise_draws(df, "ess_tail")[3,2])
  
  
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
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_cumlogit_ppo1[k] <- as.numeric(summarise_draws(df, "median")[3,2]) - log(0.8)
  bias_cumlogit_ppo2[k] <- as.numeric(summarise_draws(phi1, "median")[1,2]) - (log(0.8) + 0.06)
  
  
  # Coverage - does the true value fall in the credible interval?
  lci1 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[3,2])
  uci1 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[3,3])
  
  lci2 <- as.numeric(summarise_draws(phi1, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci2 <- as.numeric(summarise_draws(phi1, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cv_cumlogit_ppo1[k] <- ifelse(lci1 < log(0.8) & uci1 > log(0.8), 1, 0)
  cv_cumlogit_ppo2[k] <- ifelse(lci2 < (log(0.8) + 0.06) & uci2 > (log(0.8) + 0.06), 1, 0)
  
  
  # MSE 
  mse_cumlogit_ppo1[k] <- (as.numeric(summarise_draws(df, "median")[3,2]) - log(0.8))^2
  mse_cumlogit_ppo2[k] <- (as.numeric(summarise_draws(phi1, "median")[1,2]) - (log(0.8) + 0.06))^2
  
  
  # MCSE 
  mcse_cumlogit_ppo1[k] <- as.numeric(summarise_draws(df, "mcse_median")[3,2])
  mcse_cumlogit_ppo2[k] <- as.numeric(summarise_draws(phi1, "mcse_median")[1,2])
  
  
  # R-hat 
  rhat_cumlogit_ppo1[k] <- as.numeric(summarise_draws(df, "rhat")[3,2])
  rhat_cumlogit_ppo2[k] <- as.numeric(summarise_draws(phi1, "rhat")[1,2])
  
  
  # Bulk ESS 
  bulkess_cumlogit_ppo1[k] <- as.numeric(summarise_draws(df, "ess_bulk")[3,2])
  bulkess_cumlogit_ppo2[k] <- as.numeric(summarise_draws(phi1, "ess_bulk")[1,2])
  
  
  # Tail ESS 
  tailess_cumlogit_ppo1[k] <- as.numeric(summarise_draws(df, "ess_tail")[3,2])
  tailess_cumlogit_ppo2[k] <- as.numeric(summarise_draws(phi1, "ess_tail")[1,2])
  
  
  # Number of divergent transitions 
  numdivergent_ppo[k] <- get_num_divergent(stanGet(ppomod))
  
  
  # Number of missing values
  missing_ppo[k] <- length(ppomod$na.action)
  
  
  ################### CONSTRAINED PPO MODEL ASSUMING LINEARITY ################
  cppomodlin <- blrm(y~x,ppo = ~x, normcppo=T,
                     priorsdppo = 100,
                     conc = 1, seed=1234,iter=7500,chains=4,cppo=function(y) y,sampling.args = list(cores = 4))
  
  h <- cppomodlin$cppo  
  
  df <- as_draws_df(cppomodlin$draws)
  
  phi1 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(2))
  phi1 <- subset_draws(phi1, c("phi"))
  
  phi2 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(3))
  phi2 <- subset_draws(phi2, c("phi"))
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_cumlogit_lcpo1[k] <- as.numeric(summarise_draws(phi1, "median")[1,2]) - log(0.8)
  bias_cumlogit_lcpo2[k] <- as.numeric(summarise_draws(phi2, "median")[1,2]) - (log(0.8) + 0.06)
  
  
  # Coverage - does the true value fall in the credible interval?
  lci1 <- as.numeric(summarise_draws(phi1, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci1 <- as.numeric(summarise_draws(phi1, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  lci2 <- as.numeric(summarise_draws(phi2, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci2 <- as.numeric(summarise_draws(phi2, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cv_cumlogit_lcpo1[k] <- ifelse(lci1 < log(0.8) & uci1 > log(0.8), 1, 0)
  cv_cumlogit_lcpo2[k] <- ifelse(lci2 < (log(0.8) + 0.06) & uci2 > (log(0.8) + 0.06), 1, 0)
  
  
  # MSE 
  mse_cumlogit_lcpo1[k] <- (as.numeric(summarise_draws(phi1, "median")[1,2]) - log(0.8))^2
  mse_cumlogit_lcpo2[k] <- (as.numeric(summarise_draws(phi2, "median")[1,2]) - (log(0.8) + 0.06))^2
  
  
  # MCSE 
  mcse_cumlogit_lcpo1[k] <- as.numeric(summarise_draws(phi1, "mcse_median")[1,2])
  mcse_cumlogit_lcpo2[k] <- as.numeric(summarise_draws(phi2, "mcse_median")[1,2])
  
  
  # R-hat 
  rhat_cumlogit_lcpo1[k] <- as.numeric(summarise_draws(phi1, "rhat")[1,2])
  rhat_cumlogit_lcpo2[k] <- as.numeric(summarise_draws(phi2, "rhat")[1,2])
  
  
  # Bulk ESS 
  bulkess_cumlogit_lcpo1[k] <- as.numeric(summarise_draws(phi1, "ess_bulk")[1,2])
  bulkess_cumlogit_lcpo2[k] <- as.numeric(summarise_draws(phi2, "ess_bulk")[1,2])
  
  
  # Tail ESS 
  tailess_cumlogit_lcpo1[k] <- as.numeric(summarise_draws(phi1, "ess_tail")[1,2])
  tailess_cumlogit_lcpo2[k] <- as.numeric(summarise_draws(phi2, "ess_tail")[1,2])
  
  
  # Number of divergent transitions 
  numdivergent_lcpo[k] <- get_num_divergent(stanGet(cppomodlin))
  
  
  # Number of missing values
  missing_lcpo[k] <- length(cppomodlin$na.action)
  
  
  ################### CONSTRAINED PPO MODEL ASSUMING DIVERGENCE ################
  
  cppomoddiv <- blrm(y~x,ppo = ~x, normcppo=F,
                     priorsdppo = 100,
                     conc = 1, seed=1234,iter=7500,chains=4,cppo=function(y) y==3,sampling.args = list(cores = 4))
  
  h <- cppomoddiv$cppo  
  
  df <- as_draws_df(cppomoddiv$draws)
  
  phi1 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(2))
  phi1 <- subset_draws(phi1, c("phi"))
  
  phi2 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(3))
  phi2 <- subset_draws(phi2, c("phi"))
  
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_cumlogit_cpo1[k] <- as.numeric(summarise_draws(phi1, "median")[1,2]) - log(0.8)
  bias_cumlogit_cpo2[k] <- as.numeric(summarise_draws(phi2, "median")[1,2]) - (log(0.8) + 0.06)
  
  
  # Coverage - does the true value fall in the credible interval?
  lci1 <- as.numeric(summarise_draws(phi1, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci1 <- as.numeric(summarise_draws(phi1, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  lci2 <- as.numeric(summarise_draws(phi2, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci2 <- as.numeric(summarise_draws(phi2, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cv_cumlogit_cpo1[k] <- ifelse(lci1 < log(0.8) & uci1 > log(0.8), 1, 0)
  cv_cumlogit_cpo2[k] <- ifelse(lci2 < (log(0.8) + 0.06) & uci2 > (log(0.8) + 0.06), 1, 0)
  
  
  # MSE 
  mse_cumlogit_cpo1[k] <- (as.numeric(summarise_draws(phi1, "median")[1,2]) - log(0.8))^2
  mse_cumlogit_cpo2[k] <- (as.numeric(summarise_draws(phi2, "median")[1,2]) - (log(0.8) + 0.06))^2
  
  
  # MCSE 
  mcse_cumlogit_cpo1[k] <- as.numeric(summarise_draws(phi1, "mcse_median")[1,2])
  mcse_cumlogit_cpo2[k] <- as.numeric(summarise_draws(phi2, "mcse_median")[1,2])
  
  
  # R-hat 
  rhat_cumlogit_cpo1[k] <- as.numeric(summarise_draws(phi1, "rhat")[1,2])
  rhat_cumlogit_cpo2[k] <- as.numeric(summarise_draws(phi2, "rhat")[1,2])
  
  
  # Bulk ESS 
  bulkess_cumlogit_cpo1[k] <- as.numeric(summarise_draws(phi1, "ess_bulk")[1,2])
  bulkess_cumlogit_cpo2[k] <- as.numeric(summarise_draws(phi2, "ess_bulk")[1,2])
  
  
  # Tail ESS 
  tailess_cumlogit_cpo1[k] <- as.numeric(summarise_draws(phi1, "ess_tail")[1,2])
  tailess_cumlogit_cpo2[k] <- as.numeric(summarise_draws(phi2, "ess_tail")[1,2])
  
  
  # Number of divergent transitions 
  numdivergent_cpo[k] <- get_num_divergent(stanGet(cppomoddiv))
  
  
  # Number of missing values
  missing_cpo[k] <- length(cppomoddiv$na.action)
  
  
  ################### LOGISTIC MODEL - FIRST CUMULATIVE LOGIT (cat 3 and 2 vs 1) ################
  logmod1 <- blrm(y1~x1, 
                  pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                 contrast=expression(c2-c1), sd=100),
                  conc = 1, seed=1234,iter=7500,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(logmod1$draws)

  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_cumlogit_lr1[k] <- as.numeric(summarise_draws(df, "median")[2,2]) - log(0.8)
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  cv_cumlogit_lr1[k] <- ifelse(lci < log(0.8) & uci > log(0.8), 1, 0)
  
  # MSE 
  mse_cumlogit_lr1[k] <- (as.numeric(summarise_draws(df, "median")[2,2]) - log(0.8))^2

  
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
  
  
  ################### LOGISTIC MODEL - SECOND CUMULATIVE LOGIT (cat 3 vs 2 and 1) ################
  logmod2 <- blrm(y2~x1, 
                  pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                 contrast=expression(c2-c1), sd=100),
                  conc = 1, seed=1234,iter=7500,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(logmod2$draws)
  
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_cumlogit_lr2[k] <- as.numeric(summarise_draws(df, "median")[2,2]) - (log(0.8) + 0.06)
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  cv_cumlogit_lr2[k] <- ifelse(lci < (log(0.8) + 0.06) & uci > (log(0.8) + 0.06), 1, 0)
  
  # MSE 
  mse_cumlogit_lr2[k] <- (as.numeric(summarise_draws(df, "median")[2,2]) - (log(0.8) + 0.06))^2
  
  
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
  
}


## Format data for analysis 
data <- data.frame(bias_cumlogit_cpo1 = bias_cumlogit_cpo1, bias_cumlogit_cpo2 = bias_cumlogit_cpo2,
                   bias_cumlogit_lcpo1 = bias_cumlogit_lcpo1,bias_cumlogit_lcpo2 = bias_cumlogit_lcpo2,
                   bias_cumlogit_lr1 = bias_cumlogit_lr1,bias_cumlogit_lr2 = bias_cumlogit_lr2,
                   bias_cumlogit_po1 = bias_cumlogit_po1,bias_cumlogit_po2 = bias_cumlogit_po2,
                   bias_cumlogit_ppo1 = bias_cumlogit_ppo1,bias_cumlogit_ppo2 = bias_cumlogit_ppo2,
                   cv_cumlogit_cpo1 = cv_cumlogit_cpo1, cv_cumlogit_cpo2 = cv_cumlogit_cpo2,
                   cv_cumlogit_lcpo1 = cv_cumlogit_lcpo1,cv_cumlogit_lcpo2 = cv_cumlogit_lcpo2,
                   cv_cumlogit_lr1 = cv_cumlogit_lr1,cv_cumlogit_lr2 = cv_cumlogit_lr2,
                   cv_cumlogit_po1 = cv_cumlogit_po1,cv_cumlogit_po2 = cv_cumlogit_po2,
                   cv_cumlogit_ppo1 = cv_cumlogit_ppo1,cv_cumlogit_ppo2 = cv_cumlogit_ppo2,
                   mcse_cumlogit_cpo1 = mcse_cumlogit_cpo1, mcse_cumlogit_cpo2 = mcse_cumlogit_cpo2,
                   mcse_cumlogit_lcpo1 = mcse_cumlogit_lcpo1,mcse_cumlogit_lcpo2 = mcse_cumlogit_lcpo2,
                   mcse_cumlogit_lr1 = mcse_cumlogit_lr1,mcse_cumlogit_lr2 = mcse_cumlogit_lr2,
                   mcse_cumlogit_po1 = mcse_cumlogit_po1,mcse_cumlogit_po2 = mcse_cumlogit_po2,
                   mcse_cumlogit_ppo1 = mcse_cumlogit_ppo1,mcse_cumlogit_ppo2 = mcse_cumlogit_ppo2,
                   bulkess_cumlogit_cpo1 = bulkess_cumlogit_cpo1, bulkess_cumlogit_cpo2 = bulkess_cumlogit_cpo2,
                   bulkess_cumlogit_lcpo1 = bulkess_cumlogit_lcpo1,bulkess_cumlogit_lcpo2 = bulkess_cumlogit_lcpo2,
                   bulkess_cumlogit_lr1 = bulkess_cumlogit_lr1,bulkess_cumlogit_lr2 = bulkess_cumlogit_lr2,
                   bulkess_cumlogit_po1 = bulkess_cumlogit_po1,bulkess_cumlogit_po2 = bulkess_cumlogit_po2,
                   bulkess_cumlogit_ppo1 = bulkess_cumlogit_ppo1,bulkess_cumlogit_ppo2 = bulkess_cumlogit_ppo2,
                   tailess_cumlogit_cpo1 = tailess_cumlogit_cpo1, tailess_cumlogit_cpo2 = tailess_cumlogit_cpo2,
                   tailess_cumlogit_lcpo1 = tailess_cumlogit_lcpo1,tailess_cumlogit_lcpo2 = tailess_cumlogit_lcpo2,
                   tailess_cumlogit_lr1 = tailess_cumlogit_lr1,tailess_cumlogit_lr2 = tailess_cumlogit_lr2,
                   tailess_cumlogit_po1 = tailess_cumlogit_po1,tailess_cumlogit_po2 = tailess_cumlogit_po2,
                   tailess_cumlogit_ppo1 = tailess_cumlogit_ppo1,tailess_cumlogit_ppo2 = tailess_cumlogit_ppo2,
                   mse_cumlogit_cpo1 = mse_cumlogit_cpo1, mse_cumlogit_cpo2 = mse_cumlogit_cpo2,
                   mse_cumlogit_lcpo1 = mse_cumlogit_lcpo1,mse_cumlogit_lcpo2 = mse_cumlogit_lcpo2,
                   mse_cumlogit_lr1 = mse_cumlogit_lr1,mse_cumlogit_lr2 = mse_cumlogit_lr2,
                   mse_cumlogit_po1 = mse_cumlogit_po1,mse_cumlogit_po2 = mse_cumlogit_po2,
                   mse_cumlogit_ppo1 = mse_cumlogit_ppo1,mse_cumlogit_ppo2 = mse_cumlogit_ppo2,
                   rhat_cumlogit_cpo1 = rhat_cumlogit_cpo1, rhat_cumlogit_cpo2 = rhat_cumlogit_cpo2,
                   rhat_cumlogit_lcpo1 = rhat_cumlogit_lcpo1,rhat_cumlogit_lcpo2 = rhat_cumlogit_lcpo2,
                   rhat_cumlogit_lr1 = rhat_cumlogit_lr1,rhat_cumlogit_lr2 = rhat_cumlogit_lr2,
                   rhat_cumlogit_po1 = rhat_cumlogit_po1,rhat_cumlogit_po2 = rhat_cumlogit_po2,
                   rhat_cumlogit_ppo1 = rhat_cumlogit_ppo1,rhat_cumlogit_ppo2 = rhat_cumlogit_ppo2,
                   missing_cpo = missing_cpo, missing_lcpo = missing_lcpo,
                   missing_lr1 = missing_lr1,missing_lr2 = missing_lr2,
                   missing_po = missing_po,missing_ppo = missing_ppo,
                   numdivergent_cpo = numdivergent_cpo, numdivergent_lcpo = numdivergent_lcpo,
                   numdivergent_lr1 = numdivergent_lr1,numdivergent_lr2 = numdivergent_lr2,
                   numdivergent_po = numdivergent_po,numdivergent_ppo = numdivergent_ppo
)  

data <- data[s1[datnum]:s2[datnum],]

save(data, file = paste0("largen_skew_3cat_linear",datnum,".Rdata"))

end <- Sys.time()
start-end


