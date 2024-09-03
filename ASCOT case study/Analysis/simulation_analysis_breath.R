## Set working directory 
setwd("H:/PhD/Project_2/ASCOT")

## Load in the appropriate packages
library("rstan")
library('posterior')
library("rmsb")
library(readxl)

# Load the analysis set 
d <- read_xlsx("analysis.xlsx")

d <- na.omit(d)

## Breathlessness scale 
x1 = as.factor(d$trt)
x = d$trt
y1 = ifelse(d$breathscale == 0,0,1)
y2 = ifelse(d$breathscale == 0 | d$breathscale == 1,0,1)
y3 = ifelse(d$breathscale == 1 | d$breathscale == 2 | d$breathscale == 0,0,1) 
y4 = ifelse(d$breathscale == 1 | d$breathscale == 2 | d$breathscale == 3 | d$breathscale == 0,0,1)

. <- function(...) list(...)

  
  ##################### Proportional odds model ##################
  pomod <- blrm(d$breathscale~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                  contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(pomod$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_po1 <- as.numeric(summarise_draws(df, "median")[5,2])
  cumlogit_po2 <- as.numeric(summarise_draws(df, "median")[5,2])
  cumlogit_po3 <- as.numeric(summarise_draws(df, "median")[5,2])
  cumlogit_po4 <- as.numeric(summarise_draws(df, "median")[5,2]) 

  
  # Credible interval
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[5,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[5,3])

  cil_cumlogit_po1 <- lci
  cil_cumlogit_po2 <- lci
  cil_cumlogit_po3 <- lci
  cil_cumlogit_po4 <- lci
 
  ciu_cumlogit_po1 <- uci
  ciu_cumlogit_po2 <- uci
  ciu_cumlogit_po3 <- uci
  ciu_cumlogit_po4 <- uci
  
  
  
  ############### Unconstrained partial proportional odds model ###################
  ppomod <- blrm(d$breathscale~x,ppo = ~x, priorsdppo = 100,
                 conc = 1, seed=1234,iter=7500,chains=4,sampling.args = list(cores = 4,control=list(adapt_delta=0.99,max_treedepth=12)))
  
  df <- as_draws_df(ppomod$draws)
  
  phi1 <- mutate_variables(df, phi = df$x + df$`x:y>=2`)
  phi1 <- subset_draws(phi1, c("phi"))
  
  phi2 <- mutate_variables(df, phi = df$x + df$`x:y>=3`)
  phi2 <- subset_draws(phi2, c("phi"))
  
  phi3 <- mutate_variables(df, phi = df$x + df$`x:y>=4`)
  phi3 <- subset_draws(phi3, c("phi"))

  
  # Extract summary for proportional OR 
  # Point estimate
  cumlogit_ppo1 <- as.numeric(summarise_draws(df, "median")[5,2])
  cumlogit_ppo2 <- as.numeric(summarise_draws(phi1, "median")[1,2])
  cumlogit_ppo3 <- as.numeric(summarise_draws(phi2, "median")[1,2])
  cumlogit_ppo4 <- as.numeric(summarise_draws(phi3, "median")[1,2]) 
  
  
  # Coverage - does the true value fall in the credible interval?
  cil_cumlogit_ppo1 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[5,2])
  ciu_cumlogit_ppo1 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[5,3])
  
  cil_cumlogit_ppo2 <- as.numeric(summarise_draws(phi1, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo2 <- as.numeric(summarise_draws(phi1, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo3 <- as.numeric(summarise_draws(phi2, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo3 <- as.numeric(summarise_draws(phi2, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo4 <- as.numeric(summarise_draws(phi3, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo4 <- as.numeric(summarise_draws(phi3, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  
  
  ################### CONSTRAINED PPO MODEL ASSUMING LINEARITY ################
  cppomodlin <- blrm(d$breathscale~x,ppo = ~x, normcppo=T,
                     priorsdppo = 100,
                     conc = 1, seed=1234,iter=4000,chains=4,cppo=function(y) y,
                     sampling.args = list(cores = 4,control=list(adapt_delta=0.99,max_treedepth=12)))

  h <- cppomodlin$cppo  
  
  df <- as_draws_df(cppomodlin$draws)
  
  phi1 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(1))
  phi1 <- subset_draws(phi1, c("phi"))
  
  phi2 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(2))
  phi2 <- subset_draws(phi2, c("phi"))
  
  phi3 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(3))
  phi3 <- subset_draws(phi3, c("phi"))
  
  phi4 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(4))
  phi4 <- subset_draws(phi4, c("phi"))
  
  
  
  # Extract summary for proportional OR 
  # Estimate for each cumulative logit 
  cumlogit_lcpo1 <- as.numeric(summarise_draws(phi1, "median")[1,2])
  cumlogit_lcpo2 <- as.numeric(summarise_draws(phi2, "median")[1,2])
  cumlogit_lcpo3 <- as.numeric(summarise_draws(phi3, "median")[1,2]) 
  cumlogit_lcpo4 <- as.numeric(summarise_draws(phi4, "median")[1,2]) 
  
  
  # Coverage - does the true value fall in the credible interval?
  cil_cumlogit_lcpo1 <- as.numeric(summarise_draws(phi1, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo1 <- as.numeric(summarise_draws(phi1, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo2 <- as.numeric(summarise_draws(phi2, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo2 <- as.numeric(summarise_draws(phi2, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo3 <- as.numeric(summarise_draws(phi3, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo3 <- as.numeric(summarise_draws(phi3, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo4 <- as.numeric(summarise_draws(phi4, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo4 <- as.numeric(summarise_draws(phi4, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  
  
  ################### CONSTRAINED PPO MODEL ASSUMING DIVERGENCE ################
  
  cppomoddiv <- blrm(d$breathscale~x,ppo = ~x, normcppo=F,
                     priorsdppo = 100,
                     conc = 1, seed=1234,iter=4000,chains=4,cppo=function(y) y==4,
                     sampling.args = list(cores = 4,control=list(adapt_delta=0.99,max_treedepth=12)))
  
  h <- cppomoddiv$cppo  
  
  df <- as_draws_df(cppomoddiv$draws)
  
  
  phi1 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(1))
  phi1 <- subset_draws(phi1, c("phi"))
  
  phi2 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(2))
  phi2 <- subset_draws(phi2, c("phi"))
  
  phi3 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(3))
  phi3 <- subset_draws(phi3, c("phi"))
  
  phi4 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(4))
  phi4 <- subset_draws(phi4, c("phi"))
  
  
  
  # Extract summary for proportional OR 
  # Estimate for each cumulative logit 
  cumlogit_cpo1 <- as.numeric(summarise_draws(phi1, "median")[1,2])
  cumlogit_cpo2 <- as.numeric(summarise_draws(phi2, "median")[1,2])
  cumlogit_cpo3 <- as.numeric(summarise_draws(phi3, "median")[1,2]) 
  cumlogit_cpo4 <- as.numeric(summarise_draws(phi4, "median")[1,2]) 
  
  
  # Coverage - does the true value fall in the credible interval?
  cil_cumlogit_cpo1 <- as.numeric(summarise_draws(phi1, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo1 <- as.numeric(summarise_draws(phi1, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo2 <- as.numeric(summarise_draws(phi2, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo2 <- as.numeric(summarise_draws(phi2, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo3 <- as.numeric(summarise_draws(phi3, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo3 <- as.numeric(summarise_draws(phi3, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo4 <- as.numeric(summarise_draws(phi4, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo4 <- as.numeric(summarise_draws(phi4, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  

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
  
  
  ## Create data frame 
  est <- c(cumlogit_po1,cumlogit_po2, cumlogit_po3, cumlogit_po4, cumlogit_ppo1,cumlogit_ppo2,cumlogit_ppo3,cumlogit_ppo4,
           cumlogit_lcpo1,cumlogit_lcpo2,cumlogit_lcpo3,cumlogit_lcpo4,cumlogit_cpo1,cumlogit_cpo2,cumlogit_cpo3,cumlogit_cpo4,
           cumlogit_lr1,cumlogit_lr2,cumlogit_lr3,cumlogit_lr4)
  
  lci <- c(cil_cumlogit_po1,cil_cumlogit_po2,cil_cumlogit_po3,cil_cumlogit_po4,cil_cumlogit_ppo1,cil_cumlogit_ppo2,cil_cumlogit_ppo3,
           cil_cumlogit_ppo4,cil_cumlogit_lcpo1,cil_cumlogit_lcpo2,cil_cumlogit_lcpo3,cil_cumlogit_lcpo4,cil_cumlogit_cpo1,
           cil_cumlogit_cpo2,cil_cumlogit_cpo3,cil_cumlogit_cpo4,cil_cumlogit_lr1,cil_cumlogit_lr2,cil_cumlogit_lr3,cil_cumlogit_lr4)
  
  uci <- c(ciu_cumlogit_po1,ciu_cumlogit_po2,ciu_cumlogit_po3,ciu_cumlogit_po4,ciu_cumlogit_ppo1,ciu_cumlogit_ppo2,ciu_cumlogit_ppo3,
           ciu_cumlogit_ppo4,ciu_cumlogit_lcpo1,ciu_cumlogit_lcpo2,ciu_cumlogit_lcpo3,ciu_cumlogit_lcpo4,ciu_cumlogit_cpo1,
           ciu_cumlogit_cpo2,ciu_cumlogit_cpo3,ciu_cumlogit_cpo4,ciu_cumlogit_lr1,ciu_cumlogit_lr2,ciu_cumlogit_lr3,ciu_cumlogit_lr4)
  
  Method <- c(rep("PO",4), rep("Unconstrained PPO",4), rep("Linear CPPO", 4), rep("CPPO (divergent OR)", 4), rep("LR", 4))
  
  cumlogit <- c(1:4,1:4,1:4,1:4,1:4)
  
  data <- data.frame(est = est, lci = lci, uci = uci, method = method, cumlogit = cumlogit)
  
  library(ggplot2)
  pd <- position_dodge(0.68)
  
  ggplot(data, aes(x=cumlogit, y = est, group = Method, color = Method)) +
    #draws the means
    geom_point(position=pd) +
    #draws the CI error bars
    geom_errorbar(data=data, aes(ymin=lci, ymax=uci, 
                                  color=Method), width=.5, position=pd)  +
    labs(x = "Cumulative logit", y = "Log-Odds Ratio", fill = "Method") +
    geom_hline(yintercept = 0, linetype = "twodash") 
  ggsave("casestudy.png",width = 12, height = 12)
  
  