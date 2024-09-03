## Set working directory 
setwd("H:/PhD/Project_2/ASCOT")

## Load in the appropriate packages
library("rstan")
library('posterior')
library("rmsb")
library(readxl)

# Load the analysis set 
d <- read_xlsx("analysis.xlsx")

#d <- na.omit(d)

## WHO scale 
x1 = as.factor(d$trt)
x = d$trt
y1 = ifelse(d$who == 1,0,1)
y2 = ifelse(d$who == 1 | d$who == 2,0,1)
y3 = ifelse(d$who == 1 | d$who == 2 | d$who == 3,0,1) 
y4 = ifelse(d$who == 1 | d$who == 2 | d$who == 3 | d$who == 4,0,1)
y5 = ifelse(d$who == 1 | d$who == 2 | d$who == 3 | d$who == 4 | d$who == 5,0,1)
y6 = ifelse(d$who == 1 | d$who == 2 | d$who == 3 | d$who == 4 | d$who == 5 | d$who == 6,0,1)
y7 = ifelse(d$who == 1 | d$who == 2 | d$who == 3 | d$who == 4 | d$who == 5 | d$who == 6| d$who == 7,0,1)

. <- function(...) list(...)

  
  ##################### Proportional odds model ##################
  pomod <- blrm(d$who~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                  contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(pomod$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_po1 <- as.numeric(summarise_draws(df, "median")[8,2])
  cumlogit_po2 <- as.numeric(summarise_draws(df, "median")[8,2])
  cumlogit_po3 <- as.numeric(summarise_draws(df, "median")[8,2])
  cumlogit_po4 <- as.numeric(summarise_draws(df, "median")[8,2]) 
  cumlogit_po5 <- as.numeric(summarise_draws(df, "median")[8,2]) 
  cumlogit_po6 <- as.numeric(summarise_draws(df, "median")[8,2]) 
  cumlogit_po7 <- as.numeric(summarise_draws(df, "median")[8,2]) 
  
  
  # Credible interval
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[8,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[8,3])

  cil_cumlogit_po1 <- lci
  cil_cumlogit_po2 <- lci
  cil_cumlogit_po3 <- lci
  cil_cumlogit_po4 <- lci
  cil_cumlogit_po5 <- lci
  cil_cumlogit_po6 <- lci
  cil_cumlogit_po7 <- lci

  ciu_cumlogit_po1 <- uci
  ciu_cumlogit_po2 <- uci
  ciu_cumlogit_po3 <- uci
  ciu_cumlogit_po4 <- uci
  ciu_cumlogit_po5 <- uci
  ciu_cumlogit_po6 <- uci
  ciu_cumlogit_po7 <- uci
  
  
  
  ############### Unconstrained partial proportional odds model ###################
  ppomod <- blrm(d$who~x,ppo = ~x, priorsdppo = 100,
                 conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4,control=list(adapt_delta=0.99,max_treedepth=12)))
  
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
  
  
  # Extract summary for proportional OR 
  # Point estimate
  cumlogit_ppo1 <- as.numeric(summarise_draws(df, "median")[8,2])
  cumlogit_ppo2 <- as.numeric(summarise_draws(phi1, "median")[1,2])
  cumlogit_ppo3 <- as.numeric(summarise_draws(phi2, "median")[1,2])
  cumlogit_ppo4 <- as.numeric(summarise_draws(phi3, "median")[1,2]) 
  cumlogit_ppo5 <- as.numeric(summarise_draws(phi4, "median")[1,2]) 
  cumlogit_ppo6 <- as.numeric(summarise_draws(phi5, "median")[1,2]) 
  cumlogit_ppo7 <- as.numeric(summarise_draws(phi6, "median")[1,2]) 

  
  # Coverage - does the true value fall in the credible interval?
  cil_cumlogit_ppo1 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[8,2])
  ciu_cumlogit_ppo1 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[8,3])
  
  cil_cumlogit_ppo2 <- as.numeric(summarise_draws(phi1, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo2 <- as.numeric(summarise_draws(phi1, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo3 <- as.numeric(summarise_draws(phi2, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo3 <- as.numeric(summarise_draws(phi2, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo4 <- as.numeric(summarise_draws(phi3, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo4 <- as.numeric(summarise_draws(phi3, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo5 <- as.numeric(summarise_draws(phi4, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo5 <- as.numeric(summarise_draws(phi4, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo6 <- as.numeric(summarise_draws(phi5, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo6 <- as.numeric(summarise_draws(phi5, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo7 <- as.numeric(summarise_draws(phi6, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo7 <- as.numeric(summarise_draws(phi6, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  
  
  ################### CONSTRAINED PPO MODEL ASSUMING LINEARITY ################
  cppomodlin <- blrm(d$who~x,ppo = ~x, normcppo=T,
                     priorsdppo = 100,
                     conc = 1, seed=1234,iter=4000,chains=4,cppo=function(y) y,
                     sampling.args = list(cores = 4,control=list(adapt_delta=0.9,max_treedepth=12)))

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
  
  # Extract summary for proportional OR 
  # Estimate for each cumulative logit 
  cumlogit_lcpo1 <- as.numeric(summarise_draws(phi1, "median")[1,2])
  cumlogit_lcpo2 <- as.numeric(summarise_draws(phi2, "median")[1,2])
  cumlogit_lcpo3 <- as.numeric(summarise_draws(phi3, "median")[1,2]) 
  cumlogit_lcpo4 <- as.numeric(summarise_draws(phi4, "median")[1,2]) 
  cumlogit_lcpo5 <- as.numeric(summarise_draws(phi5, "median")[1,2]) 
  cumlogit_lcpo6 <- as.numeric(summarise_draws(phi6, "median")[1,2]) 
  cumlogit_lcpo7 <- as.numeric(summarise_draws(phi7, "median")[1,2]) 
  
  
  # Coverage - does the true value fall in the credible interval?
  cil_cumlogit_lcpo1 <- as.numeric(summarise_draws(phi1, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo1 <- as.numeric(summarise_draws(phi1, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo2 <- as.numeric(summarise_draws(phi2, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo2 <- as.numeric(summarise_draws(phi2, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo3 <- as.numeric(summarise_draws(phi3, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo3 <- as.numeric(summarise_draws(phi3, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo4 <- as.numeric(summarise_draws(phi4, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo4 <- as.numeric(summarise_draws(phi4, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo5 <- as.numeric(summarise_draws(phi5, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo5 <- as.numeric(summarise_draws(phi5, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo6 <- as.numeric(summarise_draws(phi6, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo6 <- as.numeric(summarise_draws(phi6, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo7 <- as.numeric(summarise_draws(phi7, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo7 <- as.numeric(summarise_draws(phi7, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  
  
  ################### CONSTRAINED PPO MODEL ASSUMING DIVERGENCE ################
  
  cppomoddiv <- blrm(d$who~x,ppo = ~x, normcppo=F,
                     priorsdppo = 100,
                     conc = 1, seed=1234,iter=4000,chains=4,cppo=function(y) y==8,
                     sampling.args = list(cores = 4,control=list(adapt_delta=0.9,max_treedepth=12)))
  
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
  

  
  
  # Extract summary for proportional OR 
  # Estimate for each cumulative logit 
  cumlogit_cpo1 <- as.numeric(summarise_draws(phi1, "median")[1,2])
  cumlogit_cpo2 <- as.numeric(summarise_draws(phi2, "median")[1,2])
  cumlogit_cpo3 <- as.numeric(summarise_draws(phi3, "median")[1,2]) 
  cumlogit_cpo4 <- as.numeric(summarise_draws(phi4, "median")[1,2]) 
  cumlogit_cpo5 <- as.numeric(summarise_draws(phi5, "median")[1,2]) 
  cumlogit_cpo6 <- as.numeric(summarise_draws(phi6, "median")[1,2]) 
  cumlogit_cpo7 <- as.numeric(summarise_draws(phi7, "median")[1,2]) 
  
  
  # Coverage - does the true value fall in the credible interval?
  cil_cumlogit_cpo1 <- as.numeric(summarise_draws(phi1, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo1 <- as.numeric(summarise_draws(phi1, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo2 <- as.numeric(summarise_draws(phi2, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo2 <- as.numeric(summarise_draws(phi2, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo3 <- as.numeric(summarise_draws(phi3, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo3 <- as.numeric(summarise_draws(phi3, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo4 <- as.numeric(summarise_draws(phi4, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo4 <- as.numeric(summarise_draws(phi4, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo5 <- as.numeric(summarise_draws(phi5, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo5 <- as.numeric(summarise_draws(phi5, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo6 <- as.numeric(summarise_draws(phi6, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo6 <- as.numeric(summarise_draws(phi6, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo7 <- as.numeric(summarise_draws(phi7, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo7 <- as.numeric(summarise_draws(phi7, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])

## Logistic regression 
  lrmod1 <- blrm(y1~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                                 contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(lrmod1$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_lr1 <- as.numeric(summarise_draws(df, "median")[2,2])
  
  
  # Credible interval
  cil_cumlogit_lr1 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  ciu_cumlogit_lr1 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  
  lrmod1 <- blrm(y2~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                      contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(lrmod1$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_lr2 <- as.numeric(summarise_draws(df, "median")[2,2])
  
  
  # Credible interval
  cil_cumlogit_lr2 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  ciu_cumlogit_lr2 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  
  lrmod1 <- blrm(y3~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                      contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(lrmod1$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_lr3 <- as.numeric(summarise_draws(df, "median")[2,2])
  
  
  # Credible interval
  cil_cumlogit_lr3 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  ciu_cumlogit_lr3 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  
  lrmod1 <- blrm(y4~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                      contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(lrmod1$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_lr4 <- as.numeric(summarise_draws(df, "median")[2,2])
  
  
  # Credible interval
  cil_cumlogit_lr4 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  ciu_cumlogit_lr4 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  
  
  lrmod1 <- blrm(y5~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                      contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(lrmod1$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_lr5 <- as.numeric(summarise_draws(df, "median")[2,2])
  
  
  # Credible interval
  cil_cumlogit_lr5 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  ciu_cumlogit_lr5 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  
  
  lrmod1 <- blrm(y6~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                      contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(lrmod1$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_lr6 <- as.numeric(summarise_draws(df, "median")[2,2])
  
  
  # Credible interval
  cil_cumlogit_lr6 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  ciu_cumlogit_lr6 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  
  lrmod1 <- blrm(y7~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                      contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(lrmod1$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_lr7 <- as.numeric(summarise_draws(df, "median")[2,2])
  
  
  # Credible interval
  cil_cumlogit_lr7 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  ciu_cumlogit_lr7 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  
  ## Create data frame 
  est <- c(cumlogit_po1,cumlogit_po2, cumlogit_po3, cumlogit_po4, cumlogit_po5, cumlogit_po6, cumlogit_po7, 
           cumlogit_ppo1,cumlogit_ppo2,cumlogit_ppo3,cumlogit_ppo4, cumlogit_ppo5, cumlogit_ppo6, cumlogit_ppo7,
           cumlogit_lcpo1,cumlogit_lcpo2,cumlogit_lcpo3,cumlogit_lcpo4,cumlogit_lcpo5, cumlogit_lcpo6, cumlogit_lcpo7,
           cumlogit_cpo1,cumlogit_cpo2,cumlogit_cpo3,cumlogit_cpo4,cumlogit_cpo5, cumlogit_cpo6, cumlogit_cpo7,
           cumlogit_lr1,cumlogit_lr2,cumlogit_lr3,cumlogit_lr4,cumlogit_lr5,cumlogit_lr6,cumlogit_lr7)
  
  lci <- c(cil_cumlogit_po1,cil_cumlogit_po2,cil_cumlogit_po3,cil_cumlogit_po4,cil_cumlogit_po5,cil_cumlogit_po6,cil_cumlogit_po7,
           cil_cumlogit_ppo1,cil_cumlogit_ppo2,cil_cumlogit_ppo3,cil_cumlogit_ppo4,cil_cumlogit_ppo5,cil_cumlogit_ppo6,cil_cumlogit_ppo7,
            cil_cumlogit_lcpo1,cil_cumlogit_lcpo2,cil_cumlogit_lcpo3,cil_cumlogit_lcpo4,cil_cumlogit_lcpo5,cil_cumlogit_lcpo6,cil_cumlogit_lcpo7,
           cil_cumlogit_cpo1,cil_cumlogit_cpo2,cil_cumlogit_cpo3,cil_cumlogit_cpo4,cil_cumlogit_cpo5,cil_cumlogit_cpo6,cil_cumlogit_cpo7,
           cil_cumlogit_lr1,cil_cumlogit_lr2,cil_cumlogit_lr3,cil_cumlogit_lr4,cil_cumlogit_lr5,cil_cumlogit_lr6,cil_cumlogit_lr7)
  
  uci <- c(ciu_cumlogit_po1,ciu_cumlogit_po2,ciu_cumlogit_po3,ciu_cumlogit_po4,ciu_cumlogit_po5,ciu_cumlogit_po6,ciu_cumlogit_po7,
           ciu_cumlogit_ppo1,ciu_cumlogit_ppo2,ciu_cumlogit_ppo3,ciu_cumlogit_ppo4,ciu_cumlogit_ppo5,ciu_cumlogit_ppo6,ciu_cumlogit_ppo7,
           ciu_cumlogit_lcpo1,ciu_cumlogit_lcpo2,ciu_cumlogit_lcpo3,ciu_cumlogit_lcpo4,ciu_cumlogit_lcpo5,ciu_cumlogit_lcpo6,ciu_cumlogit_lcpo7,
           ciu_cumlogit_cpo1,ciu_cumlogit_cpo2,ciu_cumlogit_cpo3,ciu_cumlogit_cpo4,ciu_cumlogit_cpo5,ciu_cumlogit_cpo6,ciu_cumlogit_cpo7,
           ciu_cumlogit_lr1,ciu_cumlogit_lr2,ciu_cumlogit_lr3,ciu_cumlogit_lr4,ciu_cumlogit_lr5,ciu_cumlogit_lr6,ciu_cumlogit_lr7)
  
  Method <- c(rep("PO",7), rep("Unconstrained PPO",7), rep("Linear CPPO", 7), rep("CPPO (divergent OR)", 7), rep("LR", 7))
  
  cumlogit <- c(1:7,1:7,1:7,1:7,1:7)
  
  data <- data.frame(est = est, lci = lci, uci = uci, Method = Method, cumlogit = cumlogit)
  
  library(ggplot2)
  pd <- position_dodge(0.68)
  
  ggplot(data, aes(x=cumlogit, y = est, group = Method, color = Method)) +
    #draws the means
    geom_point(position=pd) +
    #draws the CI error bars
    geom_errorbar(data=data, aes(ymin=lci, ymax=uci, 
                                  color=Method), width=.5, position=pd)  +
    labs(x = "Cumulative logit", y = "Log-Odds Ratio", fill = "Method") +
    geom_hline(yintercept = 0, linetype = "twodash") +
    scale_x_continuous(breaks = seq(1, 7, by = 1)) + 
    scale_y_continuous(breaks = seq(-10, 20, by = 5))
  ggsave("casestudy_who.png",width = 12, height = 12)
  
  