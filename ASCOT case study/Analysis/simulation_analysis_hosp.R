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

## Hospitalisation scale 
x1 = as.factor(d$trt)
x = d$trt
y1 = ifelse(d$dayhospitalisation == 0,0,1)
y2 = ifelse(d$dayhospitalisation == 1 | d$dayhospitalisation == 0,0,1)
y3 = ifelse(d$dayhospitalisation == 1 | d$dayhospitalisation == 2 | d$dayhospitalisation == 0,0,1) 
y4 = ifelse(d$dayhospitalisation == 1 | d$dayhospitalisation == 2 | d$dayhospitalisation == 3 | d$dayhospitalisation == 0,0,1)
y5 = ifelse(d$dayhospitalisation == 1 | d$dayhospitalisation == 2 | d$dayhospitalisation == 3 | d$dayhospitalisation == 4 | d$dayhospitalisation == 0,0,1)
y6 = ifelse(d$dayhospitalisation == 1 | d$dayhospitalisation == 2 | d$dayhospitalisation == 3 | d$dayhospitalisation == 4 | d$dayhospitalisation == 5 | d$dayhospitalisation == 0,0,1)
y7 = ifelse(d$dayhospitalisation == 1 | d$dayhospitalisation == 2 | d$dayhospitalisation == 3 | d$dayhospitalisation == 4 | d$dayhospitalisation == 5 | d$dayhospitalisation == 6| d$dayhospitalisation == 0,0,1)
y8 = ifelse(d$dayhospitalisation == 1 | d$dayhospitalisation == 2 | d$dayhospitalisation == 3 | d$dayhospitalisation == 4 | d$dayhospitalisation == 5 | d$dayhospitalisation == 6| d$dayhospitalisation == 7 | d$dayhospitalisation == 0,0,1)
y9 = ifelse(d$dayhospitalisation == 1 | d$dayhospitalisation == 2 | d$dayhospitalisation == 3 | d$dayhospitalisation == 4 | d$dayhospitalisation == 5 | d$dayhospitalisation == 6| d$dayhospitalisation == 7 | d$dayhospitalisation == 0 | d$dayhospitalisation == 8,0,1)
y10 = ifelse(d$dayhospitalisation == 1 | d$dayhospitalisation == 2 | d$dayhospitalisation == 3 | d$dayhospitalisation == 4 | d$dayhospitalisation == 5 | d$dayhospitalisation == 6| d$dayhospitalisation == 7 | d$dayhospitalisation == 0 | d$dayhospitalisation == 8 | d$dayhospitalisation == 9,0,1)
y11 = ifelse(d$dayhospitalisation == 1 | d$dayhospitalisation == 2 | d$dayhospitalisation == 3 | d$dayhospitalisation == 4 | d$dayhospitalisation == 5 | d$dayhospitalisation == 6| d$dayhospitalisation == 7 | d$dayhospitalisation == 0 | d$dayhospitalisation == 8 | d$dayhospitalisation == 9 | d$dayhospitalisation == 10,0,1)
y12 = ifelse(d$dayhospitalisation == 1 | d$dayhospitalisation == 2 | d$dayhospitalisation == 3 | d$dayhospitalisation == 4 | d$dayhospitalisation == 5 | d$dayhospitalisation == 6| d$dayhospitalisation == 7 | d$dayhospitalisation == 0 | d$dayhospitalisation == 8 | d$dayhospitalisation == 9 | d$dayhospitalisation == 10 | d$dayhospitalisation == 11,0,1)
y13 = ifelse(d$dayhospitalisation == 1 | d$dayhospitalisation == 2 | d$dayhospitalisation == 3 | d$dayhospitalisation == 4 | d$dayhospitalisation == 5 | d$dayhospitalisation == 6| d$dayhospitalisation == 7 | d$dayhospitalisation == 0 | d$dayhospitalisation == 8 | d$dayhospitalisation == 9 | d$dayhospitalisation == 10 | d$dayhospitalisation == 11 | d$dayhospitalisation == 12,0,1)
y14 = ifelse(d$dayhospitalisation == 1 | d$dayhospitalisation == 2 | d$dayhospitalisation == 3 | d$dayhospitalisation == 4 | d$dayhospitalisation == 5 | d$dayhospitalisation == 6| d$dayhospitalisation == 7 | d$dayhospitalisation == 0 | d$dayhospitalisation == 8 | d$dayhospitalisation == 9 | d$dayhospitalisation == 10 | d$dayhospitalisation == 11 | d$dayhospitalisation == 12 | d$dayhospitalisation == 13,0,1)
y15 = ifelse(d$dayhospitalisation == 1 | d$dayhospitalisation == 2 | d$dayhospitalisation == 3 | d$dayhospitalisation == 4 | d$dayhospitalisation == 5 | d$dayhospitalisation == 6| d$dayhospitalisation == 7 | d$dayhospitalisation == 0 | d$dayhospitalisation == 8 | d$dayhospitalisation == 9 | d$dayhospitalisation == 10 | d$dayhospitalisation == 11 | d$dayhospitalisation == 12 | d$dayhospitalisation == 13 | d$dayhospitalisation == 14,0,1)
y16 = ifelse(d$dayhospitalisation == 1 | d$dayhospitalisation == 2 | d$dayhospitalisation == 3 | d$dayhospitalisation == 4 | d$dayhospitalisation == 5 | d$dayhospitalisation == 6| d$dayhospitalisation == 7 | d$dayhospitalisation == 0 | d$dayhospitalisation == 8 | d$dayhospitalisation == 9 | d$dayhospitalisation == 10 | d$dayhospitalisation == 11 | d$dayhospitalisation == 12 | d$dayhospitalisation == 13 | d$dayhospitalisation == 14 | d$dayhospitalisation == 15,0,1)
y17 = ifelse(d$dayhospitalisation == 1 | d$dayhospitalisation == 2 | d$dayhospitalisation == 3 | d$dayhospitalisation == 4 | d$dayhospitalisation == 5 | d$dayhospitalisation == 6| d$dayhospitalisation == 7 | d$dayhospitalisation == 0 | d$dayhospitalisation == 8 | d$dayhospitalisation == 9 | d$dayhospitalisation == 10 | d$dayhospitalisation == 11 | d$dayhospitalisation == 12 | d$dayhospitalisation == 13 | d$dayhospitalisation == 14 | d$dayhospitalisation == 15 | d$dayhospitalisation == 16,0,1)
y18 = ifelse(d$dayhospitalisation == 1 | d$dayhospitalisation == 2 | d$dayhospitalisation == 3 | d$dayhospitalisation == 4 | d$dayhospitalisation == 5 | d$dayhospitalisation == 6| d$dayhospitalisation == 7 | d$dayhospitalisation == 0 | d$dayhospitalisation == 8 | d$dayhospitalisation == 9 | d$dayhospitalisation == 10 | d$dayhospitalisation == 11 | d$dayhospitalisation == 12 | d$dayhospitalisation == 13 | d$dayhospitalisation == 14 | d$dayhospitalisation == 15 | d$dayhospitalisation == 16 | d$dayhospitalisation == 17,0,1)
y19 = ifelse(d$dayhospitalisation == 1 | d$dayhospitalisation == 2 | d$dayhospitalisation == 3 | d$dayhospitalisation == 4 | d$dayhospitalisation == 5 | d$dayhospitalisation == 6| d$dayhospitalisation == 7 | d$dayhospitalisation == 0 | d$dayhospitalisation == 8 | d$dayhospitalisation == 9 | d$dayhospitalisation == 10 | d$dayhospitalisation == 11 | d$dayhospitalisation == 12 | d$dayhospitalisation == 13 | d$dayhospitalisation == 14 | d$dayhospitalisation == 15 | d$dayhospitalisation == 16 | d$dayhospitalisation == 17| d$dayhospitalisation == 18,0,1)
y20 = ifelse(d$dayhospitalisation == 1 | d$dayhospitalisation == 2 | d$dayhospitalisation == 3 | d$dayhospitalisation == 4 | d$dayhospitalisation == 5 | d$dayhospitalisation == 6| d$dayhospitalisation == 7 | d$dayhospitalisation == 0 | d$dayhospitalisation == 8 | d$dayhospitalisation == 9 | d$dayhospitalisation == 10 | d$dayhospitalisation == 11 | d$dayhospitalisation == 12 | d$dayhospitalisation == 13 | d$dayhospitalisation == 14 | d$dayhospitalisation == 15 | d$dayhospitalisation == 16 | d$dayhospitalisation == 17| d$dayhospitalisation == 18| d$dayhospitalisation == 19,0,1)
y21 = ifelse(d$dayhospitalisation == 1 | d$dayhospitalisation == 2 | d$dayhospitalisation == 3 | d$dayhospitalisation == 4 | d$dayhospitalisation == 5 | d$dayhospitalisation == 6| d$dayhospitalisation == 7 | d$dayhospitalisation == 0 | d$dayhospitalisation == 8 | d$dayhospitalisation == 9 | d$dayhospitalisation == 10 | d$dayhospitalisation == 11 | d$dayhospitalisation == 12 | d$dayhospitalisation == 13 | d$dayhospitalisation == 14 | d$dayhospitalisation == 15 | d$dayhospitalisation == 16 | d$dayhospitalisation == 17| d$dayhospitalisation == 18| d$dayhospitalisation == 19| d$dayhospitalisation == 20,0,1)
y22 = ifelse(d$dayhospitalisation == 1 | d$dayhospitalisation == 2 | d$dayhospitalisation == 3 | d$dayhospitalisation == 4 | d$dayhospitalisation == 5 | d$dayhospitalisation == 6| d$dayhospitalisation == 7 | d$dayhospitalisation == 0 | d$dayhospitalisation == 8 | d$dayhospitalisation == 9 | d$dayhospitalisation == 10 | d$dayhospitalisation == 11 | d$dayhospitalisation == 12 | d$dayhospitalisation == 13 | d$dayhospitalisation == 14 | d$dayhospitalisation == 15 | d$dayhospitalisation == 16 | d$dayhospitalisation == 17| d$dayhospitalisation == 18| d$dayhospitalisation == 19| d$dayhospitalisation == 20| d$dayhospitalisation == 21,0,1)
y23 = ifelse(d$dayhospitalisation == 1 | d$dayhospitalisation == 2 | d$dayhospitalisation == 3 | d$dayhospitalisation == 4 | d$dayhospitalisation == 5 | d$dayhospitalisation == 6| d$dayhospitalisation == 7 | d$dayhospitalisation == 0 | d$dayhospitalisation == 8 | d$dayhospitalisation == 9 | d$dayhospitalisation == 10 | d$dayhospitalisation == 11 | d$dayhospitalisation == 12 | d$dayhospitalisation == 13 | d$dayhospitalisation == 14 | d$dayhospitalisation == 15 | d$dayhospitalisation == 16 | d$dayhospitalisation == 17| d$dayhospitalisation == 18| d$dayhospitalisation == 19| d$dayhospitalisation == 20| d$dayhospitalisation == 21| d$dayhospitalisation == 22,0,1)
y24 = ifelse(d$dayhospitalisation == 1 | d$dayhospitalisation == 2 | d$dayhospitalisation == 3 | d$dayhospitalisation == 4 | d$dayhospitalisation == 5 | d$dayhospitalisation == 6| d$dayhospitalisation == 7 | d$dayhospitalisation == 0 | d$dayhospitalisation == 8 | d$dayhospitalisation == 9 | d$dayhospitalisation == 10 | d$dayhospitalisation == 11 | d$dayhospitalisation == 12 | d$dayhospitalisation == 13 | d$dayhospitalisation == 14 | d$dayhospitalisation == 15 | d$dayhospitalisation == 16 | d$dayhospitalisation == 17| d$dayhospitalisation == 18| d$dayhospitalisation == 19| d$dayhospitalisation == 20| d$dayhospitalisation == 21| d$dayhospitalisation == 22| d$dayhospitalisation == 23,0,1)
y25 = ifelse(d$dayhospitalisation == 1 | d$dayhospitalisation == 2 | d$dayhospitalisation == 3 | d$dayhospitalisation == 4 | d$dayhospitalisation == 5 | d$dayhospitalisation == 6| d$dayhospitalisation == 7 | d$dayhospitalisation == 0 | d$dayhospitalisation == 8 | d$dayhospitalisation == 9 | d$dayhospitalisation == 10 | d$dayhospitalisation == 11 | d$dayhospitalisation == 12 | d$dayhospitalisation == 13 | d$dayhospitalisation == 14 | d$dayhospitalisation == 15 | d$dayhospitalisation == 16 | d$dayhospitalisation == 17| d$dayhospitalisation == 18| d$dayhospitalisation == 19| d$dayhospitalisation == 20| d$dayhospitalisation == 21| d$dayhospitalisation == 22| d$dayhospitalisation == 23| d$dayhospitalisation == 24,0,1)
y26 = ifelse(d$dayhospitalisation == 1 | d$dayhospitalisation == 2 | d$dayhospitalisation == 3 | d$dayhospitalisation == 4 | d$dayhospitalisation == 5 | d$dayhospitalisation == 6| d$dayhospitalisation == 7 | d$dayhospitalisation == 0 | d$dayhospitalisation == 8 | d$dayhospitalisation == 9 | d$dayhospitalisation == 10 | d$dayhospitalisation == 11 | d$dayhospitalisation == 12 | d$dayhospitalisation == 13 | d$dayhospitalisation == 14 | d$dayhospitalisation == 15 | d$dayhospitalisation == 16 | d$dayhospitalisation == 17| d$dayhospitalisation == 18| d$dayhospitalisation == 19| d$dayhospitalisation == 20| d$dayhospitalisation == 21| d$dayhospitalisation == 22| d$dayhospitalisation == 23| d$dayhospitalisation == 24| d$dayhospitalisation == 25,0,1)
y27 = ifelse(d$dayhospitalisation == 1 | d$dayhospitalisation == 2 | d$dayhospitalisation == 3 | d$dayhospitalisation == 4 | d$dayhospitalisation == 5 | d$dayhospitalisation == 6| d$dayhospitalisation == 7 | d$dayhospitalisation == 0 | d$dayhospitalisation == 8 | d$dayhospitalisation == 9 | d$dayhospitalisation == 10 | d$dayhospitalisation == 11 | d$dayhospitalisation == 12 | d$dayhospitalisation == 13 | d$dayhospitalisation == 14 | d$dayhospitalisation == 15 | d$dayhospitalisation == 16 | d$dayhospitalisation == 17| d$dayhospitalisation == 18| d$dayhospitalisation == 19| d$dayhospitalisation == 20| d$dayhospitalisation == 21| d$dayhospitalisation == 22| d$dayhospitalisation == 23| d$dayhospitalisation == 24| d$dayhospitalisation == 25| d$dayhospitalisation == 26,0,1)
y28 = ifelse(d$dayhospitalisation == 1 | d$dayhospitalisation == 2 | d$dayhospitalisation == 3 | d$dayhospitalisation == 4 | d$dayhospitalisation == 5 | d$dayhospitalisation == 6| d$dayhospitalisation == 7 | d$dayhospitalisation == 0 | d$dayhospitalisation == 8 | d$dayhospitalisation == 9 | d$dayhospitalisation == 10 | d$dayhospitalisation == 11 | d$dayhospitalisation == 12 | d$dayhospitalisation == 13 | d$dayhospitalisation == 14 | d$dayhospitalisation == 15 | d$dayhospitalisation == 16 | d$dayhospitalisation == 17| d$dayhospitalisation == 18| d$dayhospitalisation == 19| d$dayhospitalisation == 20| d$dayhospitalisation == 21| d$dayhospitalisation == 22| d$dayhospitalisation == 23| d$dayhospitalisation == 24| d$dayhospitalisation == 25| d$dayhospitalisation == 26| d$dayhospitalisation == 27,0,1)


. <- function(...) list(...)

  
  ##################### Proportional odds model ##################
  pomod <- blrm(d$dayhospitalisation~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                  contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(pomod$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_po1 <- as.numeric(summarise_draws(df, "median")[25,2])
  cumlogit_po2 <- as.numeric(summarise_draws(df, "median")[25,2])
  cumlogit_po3 <- as.numeric(summarise_draws(df, "median")[25,2])
  cumlogit_po4 <- as.numeric(summarise_draws(df, "median")[25,2]) 
  cumlogit_po5 <- as.numeric(summarise_draws(df, "median")[25,2]) 
  cumlogit_po6 <- as.numeric(summarise_draws(df, "median")[25,2]) 
  cumlogit_po7 <- as.numeric(summarise_draws(df, "median")[25,2]) 
  cumlogit_po8 <- as.numeric(summarise_draws(df, "median")[25,2]) 
  cumlogit_po9 <- as.numeric(summarise_draws(df, "median")[25,2]) 
  cumlogit_po10 <- as.numeric(summarise_draws(df, "median")[25,2]) 
  cumlogit_po11 <- as.numeric(summarise_draws(df, "median")[25,2]) 
  cumlogit_po12 <- as.numeric(summarise_draws(df, "median")[25,2]) 
  cumlogit_po13 <- as.numeric(summarise_draws(df, "median")[25,2]) 
  cumlogit_po14 <- as.numeric(summarise_draws(df, "median")[25,2]) 
  cumlogit_po15 <- as.numeric(summarise_draws(df, "median")[25,2]) 
  cumlogit_po16 <- as.numeric(summarise_draws(df, "median")[25,2]) 
  cumlogit_po17 <- as.numeric(summarise_draws(df, "median")[25,2]) 
  cumlogit_po18 <- as.numeric(summarise_draws(df, "median")[25,2]) 
  cumlogit_po19 <- as.numeric(summarise_draws(df, "median")[25,2]) 
  cumlogit_po20 <- as.numeric(summarise_draws(df, "median")[25,2]) 
  cumlogit_po21 <- as.numeric(summarise_draws(df, "median")[25,2]) 
  cumlogit_po22 <- as.numeric(summarise_draws(df, "median")[25,2]) 
  cumlogit_po23 <- as.numeric(summarise_draws(df, "median")[25,2]) 
  cumlogit_po24 <- as.numeric(summarise_draws(df, "median")[25,2]) 
  cumlogit_po25 <- as.numeric(summarise_draws(df, "median")[25,2]) 
  cumlogit_po26 <- as.numeric(summarise_draws(df, "median")[25,2]) 
  cumlogit_po27 <- as.numeric(summarise_draws(df, "median")[25,2]) 
  cumlogit_po28 <- as.numeric(summarise_draws(df, "median")[25,2]) 
  
  
  # Credible interval
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[25,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[25,3])

  cil_cumlogit_po1 <- lci
  cil_cumlogit_po2 <- lci
  cil_cumlogit_po3 <- lci
  cil_cumlogit_po4 <- lci
  cil_cumlogit_po5 <- lci
  cil_cumlogit_po6 <- lci
  cil_cumlogit_po7 <- lci
  cil_cumlogit_po7 <- lci
  cil_cumlogit_po8 <- lci
  cil_cumlogit_po9 <- lci
  cil_cumlogit_po10 <- lci
  cil_cumlogit_po11 <- lci
  cil_cumlogit_po12 <- lci
  cil_cumlogit_po13 <- lci
  cil_cumlogit_po14 <- lci
  cil_cumlogit_po15 <- lci
  cil_cumlogit_po16 <- lci
  cil_cumlogit_po17 <- lci
  cil_cumlogit_po18 <- lci
  cil_cumlogit_po19 <- lci
  cil_cumlogit_po20 <- lci
  cil_cumlogit_po21 <- lci
  cil_cumlogit_po22 <- lci
  cil_cumlogit_po23 <- lci
  cil_cumlogit_po24 <- lci
  cil_cumlogit_po25 <- lci
  cil_cumlogit_po26 <- lci
  cil_cumlogit_po27 <- lci
  cil_cumlogit_po28 <- lci
  
  ciu_cumlogit_po1 <- uci
  ciu_cumlogit_po2 <- uci
  ciu_cumlogit_po3 <- uci
  ciu_cumlogit_po4 <- uci
  ciu_cumlogit_po5 <- uci
  ciu_cumlogit_po6 <- uci
  ciu_cumlogit_po7 <- uci
  ciu_cumlogit_po8 <- uci
  ciu_cumlogit_po9 <- uci
  ciu_cumlogit_po10 <- uci
  ciu_cumlogit_po11 <- uci
  ciu_cumlogit_po12 <- uci
  ciu_cumlogit_po13 <- uci
  ciu_cumlogit_po14 <- uci
  ciu_cumlogit_po15 <- uci
  ciu_cumlogit_po16 <- uci
  ciu_cumlogit_po17 <- uci
  ciu_cumlogit_po18 <- uci
  ciu_cumlogit_po19 <- uci
  ciu_cumlogit_po20 <- uci
  ciu_cumlogit_po21 <- uci
  ciu_cumlogit_po22 <- uci
  ciu_cumlogit_po23 <- uci
  ciu_cumlogit_po24 <- uci
  ciu_cumlogit_po25 <- uci
  ciu_cumlogit_po26 <- uci
  ciu_cumlogit_po27 <- uci
  ciu_cumlogit_po28 <- uci
  
  
  
  ############### Unconstrained partial proportional odds model ###################
  ppomod <- blrm(d$dayhospitalisation~x,ppo = ~x, priorsdppo = 100,
                 conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4,control=list(adapt_delta=0.9,max_treedepth=12)))
  
  df <- as_draws_df(ppomod$draws)
  
  phi1 <- mutate_variables(df, phi = df$x + df$`x:y>=4`)
  phi1 <- subset_draws(phi1, c("phi"))
  
  phi2 <- mutate_variables(df, phi = df$x + df$`x:y>=6`)
  phi2 <- subset_draws(phi2, c("phi"))
  
  phi3 <- mutate_variables(df, phi = df$x + df$`x:y>=7`)
  phi3 <- subset_draws(phi3, c("phi"))
  
  phi4 <- mutate_variables(df, phi = df$x + df$`x:y>=8`)
  phi4 <- subset_draws(phi4, c("phi"))
  
  phi5 <- mutate_variables(df, phi = df$x + df$`x:y>=9`)
  phi5 <- subset_draws(phi5, c("phi"))
  
  phi6 <- mutate_variables(df, phi = df$x + df$`x:y>=10`)
  phi6 <- subset_draws(phi6, c("phi"))
  
  phi7 <- mutate_variables(df, phi = df$x + df$`x:y>=11`)
  phi7 <- subset_draws(phi7, c("phi"))
  
  
  phi8 <- mutate_variables(df, phi = df$x + df$`x:y>=12`)
  phi8 <- subset_draws(phi8, c("phi"))
  
  
  phi9 <- mutate_variables(df, phi = df$x + df$`x:y>=13`)
  phi9 <- subset_draws(phi9, c("phi"))
  
  
  phi10 <- mutate_variables(df, phi = df$x + df$`x:y>=14`)
  phi10 <- subset_draws(phi10, c("phi"))
  
  
  phi11 <- mutate_variables(df, phi = df$x + df$`x:y>=15`)
  phi11 <- subset_draws(phi11, c("phi"))
  
  phi12 <- mutate_variables(df, phi = df$x + df$`x:y>=16`)
  phi12 <- subset_draws(phi12, c("phi"))
  
  phi13 <- mutate_variables(df, phi = df$x + df$`x:y>=17`)
  phi13 <- subset_draws(phi13, c("phi"))
  
  phi14 <- mutate_variables(df, phi = df$x + df$`x:y>=18`)
  phi14 <- subset_draws(phi14, c("phi"))
  
  phi15 <- mutate_variables(df, phi = df$x + df$`x:y>=19`)
  phi15 <- subset_draws(phi15, c("phi"))
  
  phi16 <- mutate_variables(df, phi = df$x + df$`x:y>=20`)
  phi16 <- subset_draws(phi16, c("phi"))
  
  
  phi17 <- mutate_variables(df, phi = df$x + df$`x:y>=21`)
  phi17 <- subset_draws(phi17, c("phi"))
  
  phi18 <- mutate_variables(df, phi = df$x + df$`x:y>=22`)
  phi18 <- subset_draws(phi18, c("phi"))
  
  phi19 <- mutate_variables(df, phi = df$x + df$`x:y>=23`)
  phi19 <- subset_draws(phi19, c("phi"))
  
  phi20 <- mutate_variables(df, phi = df$x + df$`x:y>=24`)
  phi20 <- subset_draws(phi20, c("phi"))
  
  phi21 <- mutate_variables(df, phi = df$x + df$`x:y>=25`)
  phi21 <- subset_draws(phi21, c("phi"))

  
  phi22 <- mutate_variables(df, phi = df$x + df$`x:y>=26`)
  phi22 <- subset_draws(phi22, c("phi"))
  
  
  phi23 <- mutate_variables(df, phi = df$x + df$`x:y>=27`)
  phi23 <- subset_draws(phi23, c("phi"))
  
  
  # Extract summary for proportional OR 
  # Point estimate
  cumlogit_ppo1 <- as.numeric(summarise_draws(df, "median")[25,2])
  cumlogit_ppo2 <- as.numeric(summarise_draws(df, "median")[25,2])
  cumlogit_ppo3 <- as.numeric(summarise_draws(df, "median")[25,2])
  cumlogit_ppo4 <- as.numeric(summarise_draws(phi1, "median")[1,2]) 
  cumlogit_ppo5 <- as.numeric(summarise_draws(phi1, "median")[1,2]) 
  cumlogit_ppo6 <- as.numeric(summarise_draws(phi2, "median")[1,2]) 
  cumlogit_ppo7 <- as.numeric(summarise_draws(phi3, "median")[1,2]) 
  cumlogit_ppo8 <- as.numeric(summarise_draws(phi4, "median")[1,2]) 
  cumlogit_ppo9 <- as.numeric(summarise_draws(phi5, "median")[1,2]) 
  cumlogit_ppo10 <- as.numeric(summarise_draws(phi6, "median")[1,2]) 
  cumlogit_ppo11 <- as.numeric(summarise_draws(phi7, "median")[1,2]) 
  cumlogit_ppo12 <- as.numeric(summarise_draws(phi8, "median")[1,2]) 
  cumlogit_ppo13 <- as.numeric(summarise_draws(phi9, "median")[1,2]) 
  cumlogit_ppo14 <- as.numeric(summarise_draws(phi10, "median")[1,2]) 
  cumlogit_ppo15 <- as.numeric(summarise_draws(phi11, "median")[1,2]) 
  cumlogit_ppo16 <- as.numeric(summarise_draws(phi12, "median")[1,2]) 
  cumlogit_ppo17 <- as.numeric(summarise_draws(phi13, "median")[1,2]) 
  cumlogit_ppo18 <- as.numeric(summarise_draws(phi14, "median")[1,2]) 
  cumlogit_ppo19 <- as.numeric(summarise_draws(phi15, "median")[1,2]) 
  cumlogit_ppo20 <- as.numeric(summarise_draws(phi16, "median")[1,2]) 
  cumlogit_ppo21 <- as.numeric(summarise_draws(phi17, "median")[1,2]) 
  cumlogit_ppo22 <- as.numeric(summarise_draws(phi18, "median")[1,2]) 
  cumlogit_ppo23 <- as.numeric(summarise_draws(phi19, "median")[1,2]) 
  cumlogit_ppo24 <- as.numeric(summarise_draws(phi20, "median")[1,2]) 
  cumlogit_ppo25 <- as.numeric(summarise_draws(phi21, "median")[1,2]) 
  cumlogit_ppo26 <- as.numeric(summarise_draws(phi22, "median")[1,2]) 
  cumlogit_ppo27 <- as.numeric(summarise_draws(phi23, "median")[1,2]) 
  cumlogit_ppo28 <- as.numeric(summarise_draws(phi23, "median")[1,2]) 
  
  
  # Coverage - does the true value fall in the credible interval?
  cil_cumlogit_ppo1 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[25,2])
  ciu_cumlogit_ppo1 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[25,3])
  
  cil_cumlogit_ppo2 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[25,2])
  ciu_cumlogit_ppo2 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[25,3])
  
  cil_cumlogit_ppo3 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[25,2])
  ciu_cumlogit_ppo3 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[25,3])
  
  cil_cumlogit_ppo4 <- as.numeric(summarise_draws(phi1, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo4 <- as.numeric(summarise_draws(phi1, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo5 <- as.numeric(summarise_draws(phi1, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo5 <- as.numeric(summarise_draws(phi1, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo6 <- as.numeric(summarise_draws(phi2, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo6 <- as.numeric(summarise_draws(phi2, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo7 <- as.numeric(summarise_draws(phi3, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo7 <- as.numeric(summarise_draws(phi3, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo8 <- as.numeric(summarise_draws(phi4, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo8 <- as.numeric(summarise_draws(phi4, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo9 <- as.numeric(summarise_draws(phi5, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo9 <- as.numeric(summarise_draws(phi5, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo10 <- as.numeric(summarise_draws(phi6, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo10 <- as.numeric(summarise_draws(phi6, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo11 <- as.numeric(summarise_draws(phi7, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo11 <- as.numeric(summarise_draws(phi7, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo12 <- as.numeric(summarise_draws(phi8, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo12 <- as.numeric(summarise_draws(phi8, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo13 <- as.numeric(summarise_draws(phi9, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo13 <- as.numeric(summarise_draws(phi9, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo14 <- as.numeric(summarise_draws(phi10, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo14 <- as.numeric(summarise_draws(phi10, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo15 <- as.numeric(summarise_draws(phi11, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo15 <- as.numeric(summarise_draws(phi11, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo16 <- as.numeric(summarise_draws(phi12, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo16 <- as.numeric(summarise_draws(phi12, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo17 <- as.numeric(summarise_draws(phi13, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo17 <- as.numeric(summarise_draws(phi13, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo18 <- as.numeric(summarise_draws(phi14, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo18 <- as.numeric(summarise_draws(phi14, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo19 <- as.numeric(summarise_draws(phi15, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo19 <- as.numeric(summarise_draws(phi15, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo20 <- as.numeric(summarise_draws(phi16, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo20 <- as.numeric(summarise_draws(phi16, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo21 <- as.numeric(summarise_draws(phi17, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo21 <- as.numeric(summarise_draws(phi17, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo22 <- as.numeric(summarise_draws(phi18, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo22 <- as.numeric(summarise_draws(phi18, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo23 <- as.numeric(summarise_draws(phi19, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo23 <- as.numeric(summarise_draws(phi19, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo24 <- as.numeric(summarise_draws(phi20, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo24 <- as.numeric(summarise_draws(phi20, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo25 <- as.numeric(summarise_draws(phi21, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo25 <- as.numeric(summarise_draws(phi21, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo26 <- as.numeric(summarise_draws(phi22, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo26 <- as.numeric(summarise_draws(phi22, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo27 <- as.numeric(summarise_draws(phi23, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo27 <- as.numeric(summarise_draws(phi23, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_ppo28 <- as.numeric(summarise_draws(phi23, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_ppo28 <- as.numeric(summarise_draws(phi23, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  ################### CONSTRAINED PPO MODEL ASSUMING LINEARITY ################
  cppomodlin <- blrm(d$dayhospitalisation~x,ppo = ~x, normcppo=T,
                     priorsdppo = 100,
                     conc = 1, seed=1234,iter=4000,chains=4,cppo=function(y) y,
                     sampling.args = list(cores = 4,control=list(adapt_delta=0.95,max_treedepth=12)))

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
  
  phi5 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(5))
  phi5 <- subset_draws(phi5, c("phi"))
  
  phi6 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(6))
  phi6 <- subset_draws(phi6, c("phi"))
  
  phi7 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(7))
  phi7 <- subset_draws(phi7, c("phi"))
  
  phi8 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(8))
  phi8 <- subset_draws(phi8, c("phi"))
  
  phi9 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(9))
  phi9 <- subset_draws(phi9, c("phi"))
  
  phi10 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(10))
  phi10 <- subset_draws(phi10, c("phi"))
  
  phi11 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(11))
  phi11 <- subset_draws(phi11, c("phi"))
  
  phi12 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(12))
  phi12 <- subset_draws(phi12, c("phi"))
  
  phi13 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(13))
  phi13 <- subset_draws(phi13, c("phi"))
  
  phi14 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(14))
  phi14 <- subset_draws(phi14, c("phi"))
  
  phi15 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(15))
  phi15 <- subset_draws(phi15, c("phi"))
  
  phi16 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(16))
  phi16 <- subset_draws(phi16, c("phi"))
  
  phi17 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(17))
  phi17 <- subset_draws(phi17, c("phi"))
  
  phi18 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(18))
  phi18 <- subset_draws(phi18, c("phi"))
  
  phi19 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(19))
  phi19 <- subset_draws(phi19, c("phi"))
  
  phi20 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(20))
  phi20 <- subset_draws(phi20, c("phi"))
  
  phi21 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(21))
  phi21 <- subset_draws(phi21, c("phi"))
  
  phi22 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(22))
  phi22 <- subset_draws(phi22, c("phi"))
  
  phi23 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(23))
  phi23 <- subset_draws(phi23, c("phi"))
  
  
  phi24 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(24))
  phi24 <- subset_draws(phi24, c("phi"))
  
  phi25 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(25))
  phi25 <- subset_draws(phi25, c("phi"))
  
  phi26 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(26))
  phi26 <- subset_draws(phi26, c("phi"))
  
  phi27 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(27))
  phi27 <- subset_draws(phi27, c("phi"))
  
  phi28 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(28))
  phi28 <- subset_draws(phi28, c("phi"))
  
  # Extract summary for proportional OR 
  # Estimate for each cumulative logit 
  cumlogit_lcpo1 <- as.numeric(summarise_draws(phi1, "median")[1,2])
  cumlogit_lcpo2 <- as.numeric(summarise_draws(phi2, "median")[1,2])
  cumlogit_lcpo3 <- as.numeric(summarise_draws(phi3, "median")[1,2]) 
  cumlogit_lcpo4 <- as.numeric(summarise_draws(phi4, "median")[1,2]) 
  cumlogit_lcpo5 <- as.numeric(summarise_draws(phi5, "median")[1,2]) 
  cumlogit_lcpo6 <- as.numeric(summarise_draws(phi6, "median")[1,2]) 
  cumlogit_lcpo7 <- as.numeric(summarise_draws(phi7, "median")[1,2]) 
  cumlogit_lcpo8 <- as.numeric(summarise_draws(phi8, "median")[1,2]) 
  cumlogit_lcpo9 <- as.numeric(summarise_draws(phi9, "median")[1,2]) 
  cumlogit_lcpo10 <- as.numeric(summarise_draws(phi10, "median")[1,2]) 
  cumlogit_lcpo11 <- as.numeric(summarise_draws(phi11, "median")[1,2]) 
  cumlogit_lcpo12 <- as.numeric(summarise_draws(phi12, "median")[1,2]) 
  cumlogit_lcpo13 <- as.numeric(summarise_draws(phi13, "median")[1,2]) 
  cumlogit_lcpo14 <- as.numeric(summarise_draws(phi14, "median")[1,2]) 
  cumlogit_lcpo15 <- as.numeric(summarise_draws(phi15, "median")[1,2]) 
  cumlogit_lcpo16 <- as.numeric(summarise_draws(phi16, "median")[1,2]) 
  cumlogit_lcpo17 <- as.numeric(summarise_draws(phi17, "median")[1,2]) 
  cumlogit_lcpo18 <- as.numeric(summarise_draws(phi18, "median")[1,2]) 
  cumlogit_lcpo19 <- as.numeric(summarise_draws(phi19, "median")[1,2]) 
  cumlogit_lcpo20 <- as.numeric(summarise_draws(phi20, "median")[1,2]) 
  cumlogit_lcpo21 <- as.numeric(summarise_draws(phi21, "median")[1,2]) 
  cumlogit_lcpo22 <- as.numeric(summarise_draws(phi22, "median")[1,2]) 
  cumlogit_lcpo23 <- as.numeric(summarise_draws(phi23, "median")[1,2]) 
  cumlogit_lcpo24 <- as.numeric(summarise_draws(phi24, "median")[1,2]) 
  cumlogit_lcpo25 <- as.numeric(summarise_draws(phi25, "median")[1,2]) 
  cumlogit_lcpo26 <- as.numeric(summarise_draws(phi26, "median")[1,2]) 
  cumlogit_lcpo27 <- as.numeric(summarise_draws(phi27, "median")[1,2]) 
  cumlogit_lcpo28 <- as.numeric(summarise_draws(phi28, "median")[1,2]) 
  
  
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
  
  cil_cumlogit_lcpo8 <- as.numeric(summarise_draws(phi8, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo8 <- as.numeric(summarise_draws(phi8, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo9 <- as.numeric(summarise_draws(phi9, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo9 <- as.numeric(summarise_draws(phi9, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo10 <- as.numeric(summarise_draws(phi10, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo10 <- as.numeric(summarise_draws(phi10, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo11 <- as.numeric(summarise_draws(phi11, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo11 <- as.numeric(summarise_draws(phi11, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo12 <- as.numeric(summarise_draws(phi12, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo12 <- as.numeric(summarise_draws(phi12, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo13 <- as.numeric(summarise_draws(phi13, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo13 <- as.numeric(summarise_draws(phi13, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo14 <- as.numeric(summarise_draws(phi14, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo14 <- as.numeric(summarise_draws(phi14, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo15 <- as.numeric(summarise_draws(phi15, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo15 <- as.numeric(summarise_draws(phi15, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo16 <- as.numeric(summarise_draws(phi16, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo16 <- as.numeric(summarise_draws(phi16, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo17 <- as.numeric(summarise_draws(phi17, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo17 <- as.numeric(summarise_draws(phi17, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo18 <- as.numeric(summarise_draws(phi18, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo18 <- as.numeric(summarise_draws(phi18, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo19 <- as.numeric(summarise_draws(phi19, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo19 <- as.numeric(summarise_draws(phi19, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo20 <- as.numeric(summarise_draws(phi20, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo20 <- as.numeric(summarise_draws(phi20, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo21 <- as.numeric(summarise_draws(phi21, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo21 <- as.numeric(summarise_draws(phi21, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo22 <- as.numeric(summarise_draws(phi22, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo22 <- as.numeric(summarise_draws(phi22, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo23 <- as.numeric(summarise_draws(phi23, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo23 <- as.numeric(summarise_draws(phi23, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo24 <- as.numeric(summarise_draws(phi24, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo24 <- as.numeric(summarise_draws(phi24, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo25 <- as.numeric(summarise_draws(phi25, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo25 <- as.numeric(summarise_draws(phi25, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo26 <- as.numeric(summarise_draws(phi26, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo26 <- as.numeric(summarise_draws(phi26, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo27 <- as.numeric(summarise_draws(phi27, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo27 <- as.numeric(summarise_draws(phi27, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_lcpo28 <- as.numeric(summarise_draws(phi28, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_lcpo28 <- as.numeric(summarise_draws(phi28, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  ################### CONSTRAINED PPO MODEL ASSUMING DIVERGENCE ################
  
  cppomoddiv <- blrm(d$dayhospitalisation~x,ppo = ~x, normcppo=F,
                     priorsdppo = 100,
                     conc = 1, seed=1234,iter=4000,chains=4,cppo=function(y) y==27,
                     sampling.args = list(cores = 4,control=list(adapt_delta=0.9,max_treedepth=12)))
  
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
  
  phi5 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(5))
  phi5 <- subset_draws(phi5, c("phi"))
  
  phi6 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(6))
  phi6 <- subset_draws(phi6, c("phi"))
  
  phi7 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(7))
  phi7 <- subset_draws(phi7, c("phi"))
  
  phi8 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(8))
  phi8 <- subset_draws(phi8, c("phi"))
  
  phi9 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(9))
  phi9 <- subset_draws(phi9, c("phi"))
  
  phi10 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(10))
  phi10 <- subset_draws(phi10, c("phi"))
  
  phi11 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(11))
  phi11 <- subset_draws(phi11, c("phi"))
  
  phi12 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(12))
  phi12 <- subset_draws(phi12, c("phi"))
  
  phi13 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(13))
  phi13 <- subset_draws(phi13, c("phi"))
  
  phi14 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(14))
  phi14 <- subset_draws(phi14, c("phi"))
  
  phi15 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(15))
  phi15 <- subset_draws(phi15, c("phi"))
  
  phi16 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(16))
  phi16 <- subset_draws(phi16, c("phi"))
  
  phi17 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(17))
  phi17 <- subset_draws(phi17, c("phi"))
  
  phi18 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(18))
  phi18 <- subset_draws(phi18, c("phi"))
  
  phi19 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(19))
  phi19 <- subset_draws(phi19, c("phi"))
  
  phi20 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(20))
  phi20 <- subset_draws(phi20, c("phi"))
  
  phi21 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(21))
  phi21 <- subset_draws(phi21, c("phi"))
  
  phi22 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(22))
  phi22 <- subset_draws(phi22, c("phi"))
  
  phi23 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(23))
  phi23 <- subset_draws(phi23, c("phi"))
  
  
  phi24 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(24))
  phi24 <- subset_draws(phi24, c("phi"))
  
  phi25 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(25))
  phi25 <- subset_draws(phi25, c("phi"))
  
  phi26 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(26))
  phi26 <- subset_draws(phi26, c("phi"))
  
  phi27 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(27))
  phi27 <- subset_draws(phi27, c("phi"))
  
  phi28 <- mutate_variables(df, phi = df$x + df$`x x f(y)`*h(28))
  phi28 <- subset_draws(phi28, c("phi"))
  

  
  
  # Extract summary for proportional OR 
  # Estimate for each cumulative logit 
  cumlogit_cpo1 <- as.numeric(summarise_draws(phi1, "median")[1,2])
  cumlogit_cpo2 <- as.numeric(summarise_draws(phi2, "median")[1,2])
  cumlogit_cpo3 <- as.numeric(summarise_draws(phi3, "median")[1,2]) 
  cumlogit_cpo4 <- as.numeric(summarise_draws(phi4, "median")[1,2]) 
  cumlogit_cpo5 <- as.numeric(summarise_draws(phi5, "median")[1,2]) 
  cumlogit_cpo6 <- as.numeric(summarise_draws(phi6, "median")[1,2]) 
  cumlogit_cpo7 <- as.numeric(summarise_draws(phi7, "median")[1,2]) 
  cumlogit_cpo8 <- as.numeric(summarise_draws(phi8, "median")[1,2]) 
  cumlogit_cpo9 <- as.numeric(summarise_draws(phi9, "median")[1,2]) 
  cumlogit_cpo10 <- as.numeric(summarise_draws(phi10, "median")[1,2]) 
  cumlogit_cpo11 <- as.numeric(summarise_draws(phi11, "median")[1,2]) 
  cumlogit_cpo12 <- as.numeric(summarise_draws(phi12, "median")[1,2]) 
  cumlogit_cpo13 <- as.numeric(summarise_draws(phi13, "median")[1,2]) 
  cumlogit_cpo14 <- as.numeric(summarise_draws(phi14, "median")[1,2]) 
  cumlogit_cpo15 <- as.numeric(summarise_draws(phi15, "median")[1,2]) 
  cumlogit_cpo16 <- as.numeric(summarise_draws(phi16, "median")[1,2]) 
  cumlogit_cpo17 <- as.numeric(summarise_draws(phi17, "median")[1,2]) 
  cumlogit_cpo18 <- as.numeric(summarise_draws(phi18, "median")[1,2]) 
  cumlogit_cpo19 <- as.numeric(summarise_draws(phi19, "median")[1,2]) 
  cumlogit_cpo20 <- as.numeric(summarise_draws(phi20, "median")[1,2]) 
  cumlogit_cpo21 <- as.numeric(summarise_draws(phi21, "median")[1,2]) 
  cumlogit_cpo22 <- as.numeric(summarise_draws(phi22, "median")[1,2]) 
  cumlogit_cpo23 <- as.numeric(summarise_draws(phi23, "median")[1,2]) 
  cumlogit_cpo24 <- as.numeric(summarise_draws(phi24, "median")[1,2]) 
  cumlogit_cpo25 <- as.numeric(summarise_draws(phi25, "median")[1,2]) 
  cumlogit_cpo26 <- as.numeric(summarise_draws(phi26, "median")[1,2]) 
  cumlogit_cpo27 <- as.numeric(summarise_draws(phi27, "median")[1,2]) 
  cumlogit_cpo28 <- as.numeric(summarise_draws(phi28, "median")[1,2]) 
  
  
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
  
  cil_cumlogit_cpo8 <- as.numeric(summarise_draws(phi8, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo8 <- as.numeric(summarise_draws(phi8, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo9 <- as.numeric(summarise_draws(phi9, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo9 <- as.numeric(summarise_draws(phi9, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo10 <- as.numeric(summarise_draws(phi10, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo10 <- as.numeric(summarise_draws(phi10, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo11 <- as.numeric(summarise_draws(phi11, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo11 <- as.numeric(summarise_draws(phi11, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo12 <- as.numeric(summarise_draws(phi12, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo12 <- as.numeric(summarise_draws(phi12, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo13 <- as.numeric(summarise_draws(phi13, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo13 <- as.numeric(summarise_draws(phi13, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo14 <- as.numeric(summarise_draws(phi14, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo14 <- as.numeric(summarise_draws(phi14, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo15 <- as.numeric(summarise_draws(phi15, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo15 <- as.numeric(summarise_draws(phi15, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo16 <- as.numeric(summarise_draws(phi16, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo16 <- as.numeric(summarise_draws(phi16, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo17 <- as.numeric(summarise_draws(phi17, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo17 <- as.numeric(summarise_draws(phi17, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo18 <- as.numeric(summarise_draws(phi18, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo18 <- as.numeric(summarise_draws(phi18, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo19 <- as.numeric(summarise_draws(phi19, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo19 <- as.numeric(summarise_draws(phi19, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo20 <- as.numeric(summarise_draws(phi20, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo20 <- as.numeric(summarise_draws(phi20, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo21 <- as.numeric(summarise_draws(phi21, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo21 <- as.numeric(summarise_draws(phi21, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo22 <- as.numeric(summarise_draws(phi22, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo22 <- as.numeric(summarise_draws(phi22, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo23 <- as.numeric(summarise_draws(phi23, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo23 <- as.numeric(summarise_draws(phi23, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo24 <- as.numeric(summarise_draws(phi24, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo24 <- as.numeric(summarise_draws(phi24, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo25 <- as.numeric(summarise_draws(phi25, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo25 <- as.numeric(summarise_draws(phi25, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo26 <- as.numeric(summarise_draws(phi26, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo26 <- as.numeric(summarise_draws(phi26, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo27 <- as.numeric(summarise_draws(phi27, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo27 <- as.numeric(summarise_draws(phi27, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  cil_cumlogit_cpo28 <- as.numeric(summarise_draws(phi28, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  ciu_cumlogit_cpo28 <- as.numeric(summarise_draws(phi28, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])

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
  
  lrmod1 <- blrm(y8~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                      contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(lrmod1$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_lr8 <- as.numeric(summarise_draws(df, "median")[2,2])
  
  
  # Credible interval
  cil_cumlogit_lr8 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  ciu_cumlogit_lr8 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  lrmod1 <- blrm(y9~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                      contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(lrmod1$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_lr9 <- as.numeric(summarise_draws(df, "median")[2,2])
  
  
  # Credible interval
  cil_cumlogit_lr9 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  ciu_cumlogit_lr9 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  lrmod1 <- blrm(y10~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                      contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(lrmod1$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_lr10 <- as.numeric(summarise_draws(df, "median")[2,2])
  
  
  # Credible interval
  cil_cumlogit_lr10 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  ciu_cumlogit_lr10 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  lrmod1 <- blrm(y11~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                      contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(lrmod1$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_lr11 <- as.numeric(summarise_draws(df, "median")[2,2])
  
  
  # Credible interval
  cil_cumlogit_lr11 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  ciu_cumlogit_lr11 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  lrmod1 <- blrm(y12~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                      contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(lrmod1$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_lr12 <- as.numeric(summarise_draws(df, "median")[2,2])
  
  
  # Credible interval
  cil_cumlogit_lr12 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  ciu_cumlogit_lr12 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  lrmod1 <- blrm(y13~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                      contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(lrmod1$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_lr13 <- as.numeric(summarise_draws(df, "median")[2,2])
  
  
  # Credible interval
  cil_cumlogit_lr13 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  ciu_cumlogit_lr13 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  lrmod1 <- blrm(y14~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                      contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(lrmod1$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_lr14 <- as.numeric(summarise_draws(df, "median")[2,2])
  
  
  # Credible interval
  cil_cumlogit_lr14 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  ciu_cumlogit_lr14 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  lrmod1 <- blrm(y15~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                      contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(lrmod1$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_lr15 <- as.numeric(summarise_draws(df, "median")[2,2])
  
  
  # Credible interval
  cil_cumlogit_lr15 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  ciu_cumlogit_lr15 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  lrmod1 <- blrm(y16~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                      contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(lrmod1$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_lr16 <- as.numeric(summarise_draws(df, "median")[2,2])
  
  
  # Credible interval
  cil_cumlogit_lr16 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  ciu_cumlogit_lr16 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  
  lrmod1 <- blrm(y17~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                      contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(lrmod1$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_lr17 <- as.numeric(summarise_draws(df, "median")[2,2])
  
  
  # Credible interval
  cil_cumlogit_lr17 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  ciu_cumlogit_lr17 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  lrmod1 <- blrm(y18~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                      contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(lrmod1$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_lr18 <- as.numeric(summarise_draws(df, "median")[2,2])
  
  
  # Credible interval
  cil_cumlogit_lr18 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  ciu_cumlogit_lr18 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  
  lrmod1 <- blrm(y19~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                      contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(lrmod1$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_lr19 <- as.numeric(summarise_draws(df, "median")[2,2])
  
  
  # Credible interval
  cil_cumlogit_lr19 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  ciu_cumlogit_lr19 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  lrmod1 <- blrm(y20~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                      contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(lrmod1$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_lr20 <- as.numeric(summarise_draws(df, "median")[2,2])
  
  
  # Credible interval
  cil_cumlogit_lr20 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  ciu_cumlogit_lr20 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  lrmod1 <- blrm(y21~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                      contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(lrmod1$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_lr21 <- as.numeric(summarise_draws(df, "median")[2,2])
  
  
  # Credible interval
  cil_cumlogit_lr21 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  ciu_cumlogit_lr21 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  
  
  lrmod1 <- blrm(y22~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                      contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(lrmod1$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_lr22 <- as.numeric(summarise_draws(df, "median")[2,2])
  
  
  # Credible interval
  cil_cumlogit_lr22 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  ciu_cumlogit_lr22 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  
  lrmod1 <- blrm(y23~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                      contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(lrmod1$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_lr23 <- as.numeric(summarise_draws(df, "median")[2,2])
  
  
  # Credible interval
  cil_cumlogit_lr23 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  ciu_cumlogit_lr23 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  
  lrmod1 <- blrm(y24~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                      contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(lrmod1$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_lr24 <- as.numeric(summarise_draws(df, "median")[2,2])
  
  
  # Credible interval
  cil_cumlogit_lr24 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  ciu_cumlogit_lr24 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  
  lrmod1 <- blrm(y25~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                      contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(lrmod1$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_lr25 <- as.numeric(summarise_draws(df, "median")[2,2])
  
  
  # Credible interval
  cil_cumlogit_lr25 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  ciu_cumlogit_lr25 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  
  lrmod1 <- blrm(y7~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                      contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(lrmod1$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_lr26 <- as.numeric(summarise_draws(df, "median")[2,2])
  
  
  # Credible interval
  cil_cumlogit_lr26 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  ciu_cumlogit_lr26 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  lrmod1 <- blrm(y2~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                      contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(lrmod1$draws)
  
  # Extract summary for proportional OR 
  # pOR for each cumulative logit 
  cumlogit_lr27 <- as.numeric(summarise_draws(df, "median")[2,2])
  
  
  # Credible interval
  cil_cumlogit_lr27 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,2])
  ciu_cumlogit_lr27 <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[2,3])
  
  lrmod1 <- blrm(y28~x1,pcontrast=list(c1=.(x1='0'), c2=.(x1='1'),
                                      contrast=expression(c2-c1), sd=100), conc = 1, seed=1234,iter=4000,chains=4,sampling.args = list(cores = 4))
  
  df <- as_draws_df(lrmod1$draws)
  
  ## Create data frame 
  est <- c(cumlogit_po1,cumlogit_po2, cumlogit_po3, cumlogit_po4, cumlogit_po5, cumlogit_po6, cumlogit_po7, cumlogit_po8, cumlogit_po9, cumlogit_po10,
           cumlogit_po11, cumlogit_po12, cumlogit_po13,cumlogit_po14,cumlogit_po15,cumlogit_po16,cumlogit_po17,cumlogit_po18,cumlogit_po19,cumlogit_po20,
           cumlogit_po21,cumlogit_po22,cumlogit_po23,cumlogit_po24,cumlogit_po25,cumlogit_po26,cumlogit_po27,
           cumlogit_ppo1,cumlogit_ppo2,cumlogit_ppo3,cumlogit_ppo4, cumlogit_ppo5, cumlogit_ppo6, cumlogit_ppo7,cumlogit_ppo8,cumlogit_ppo9,cumlogit_ppo10,
           cumlogit_ppo11,cumlogit_ppo12,cumlogit_ppo13,cumlogit_ppo14,cumlogit_ppo15,cumlogit_ppo16,cumlogit_ppo17,cumlogit_ppo18,cumlogit_ppo19,cumlogit_ppo20,
           cumlogit_ppo21,cumlogit_ppo22,cumlogit_ppo23,cumlogit_ppo24,cumlogit_ppo25,cumlogit_ppo26,cumlogit_ppo27,
           cumlogit_lcpo1,cumlogit_lcpo2,cumlogit_lcpo3,cumlogit_lcpo4,cumlogit_lcpo5, cumlogit_lcpo6, cumlogit_lcpo7,cumlogit_lcpo8,cumlogit_lcpo9,cumlogit_lcpo10,
           cumlogit_lcpo11,cumlogit_lcpo12,cumlogit_lcpo13,cumlogit_lcpo14,cumlogit_lcpo15,cumlogit_lcpo16,cumlogit_lcpo17,cumlogit_lcpo18,cumlogit_lcpo19,cumlogit_lcpo20,
           cumlogit_lcpo21,cumlogit_lcpo22,cumlogit_lcpo23,cumlogit_lcpo24,cumlogit_lcpo25,cumlogit_lcpo26,cumlogit_lcpo27,
           cumlogit_cpo1,cumlogit_cpo2,cumlogit_cpo3,cumlogit_cpo4,cumlogit_cpo5, cumlogit_cpo6, cumlogit_cpo7,cumlogit_cpo8,cumlogit_cpo9,cumlogit_cpo10,cumlogit_cpo11,
           cumlogit_cpo12,cumlogit_cpo13,cumlogit_cpo14,cumlogit_cpo15,cumlogit_cpo16,cumlogit_cpo17,cumlogit_cpo18,cumlogit_cpo19,cumlogit_cpo20,cumlogit_cpo21,
           cumlogit_cpo22,cumlogit_cpo23,cumlogit_cpo24,cumlogit_cpo25,cumlogit_cpo26,cumlogit_cpo27,
           cumlogit_lr1,cumlogit_lr2,cumlogit_lr3,cumlogit_lr4,cumlogit_lr5,cumlogit_lr6,cumlogit_lr7,cumlogit_lr8,cumlogit_lr9,cumlogit_lr10,cumlogit_lr11,cumlogit_lr12,
           cumlogit_lr13,cumlogit_lr14,cumlogit_lr15,cumlogit_lr16,cumlogit_lr17,cumlogit_lr18,cumlogit_lr19,cumlogit_lr20,cumlogit_lr21,cumlogit_lr22,cumlogit_lr23,
           cumlogit_lr24,cumlogit_lr25,cumlogit_lr26,cumlogit_lr27)
  
  lci <- c(cil_cumlogit_po1,cil_cumlogit_po2,cil_cumlogit_po3,cil_cumlogit_po4,cil_cumlogit_po5,cil_cumlogit_po6,cil_cumlogit_po7,cil_cumlogit_po8,cil_cumlogit_po9,cil_cumlogit_po10,
           cil_cumlogit_po11,cil_cumlogit_po12,cil_cumlogit_po13,cil_cumlogit_po14,cil_cumlogit_po15,cil_cumlogit_po16,cil_cumlogit_po17,cil_cumlogit_po18,cil_cumlogit_po19,cil_cumlogit_po20,
           cil_cumlogit_po21,cil_cumlogit_po22,cil_cumlogit_po23,cil_cumlogit_po24,cil_cumlogit_po25,cil_cumlogit_po26,cil_cumlogit_po27,
           cil_cumlogit_ppo1,cil_cumlogit_ppo2,cil_cumlogit_ppo3,cil_cumlogit_ppo4,cil_cumlogit_ppo5,cil_cumlogit_ppo6,cil_cumlogit_ppo7,cil_cumlogit_ppo8,cil_cumlogit_ppo9,cil_cumlogit_ppo10,
           cil_cumlogit_ppo11,cil_cumlogit_ppo12,cil_cumlogit_ppo13,cil_cumlogit_ppo14,cil_cumlogit_ppo15,cil_cumlogit_ppo16,cil_cumlogit_ppo17,cil_cumlogit_ppo18,cil_cumlogit_ppo19,
           cil_cumlogit_ppo20,cil_cumlogit_ppo21,cil_cumlogit_ppo22,cil_cumlogit_ppo23,cil_cumlogit_ppo24,cil_cumlogit_ppo25,cil_cumlogit_ppo26,cil_cumlogit_ppo27,
            cil_cumlogit_lcpo1,cil_cumlogit_lcpo2,cil_cumlogit_lcpo3,cil_cumlogit_lcpo4,cil_cumlogit_lcpo5,cil_cumlogit_lcpo6,cil_cumlogit_lcpo7,cil_cumlogit_lcpo8,cil_cumlogit_lcpo9,
           cil_cumlogit_lcpo10,cil_cumlogit_lcpo11,cil_cumlogit_lcpo12,cil_cumlogit_lcpo13,cil_cumlogit_lcpo14,cil_cumlogit_lcpo15,cil_cumlogit_lcpo16,cil_cumlogit_lcpo17,cil_cumlogit_lcpo18,
           cil_cumlogit_lcpo19,cil_cumlogit_lcpo20,cil_cumlogit_lcpo21,cil_cumlogit_lcpo22,cil_cumlogit_lcpo23,cil_cumlogit_lcpo24,cil_cumlogit_lcpo25,cil_cumlogit_lcpo26,cil_cumlogit_lcpo27,
           cil_cumlogit_cpo1,cil_cumlogit_cpo2,cil_cumlogit_cpo3,cil_cumlogit_cpo4,cil_cumlogit_cpo5,cil_cumlogit_cpo6,cil_cumlogit_cpo7,cil_cumlogit_cpo8,cil_cumlogit_cpo9,cil_cumlogit_cpo10,cil_cumlogit_cpo11,
           cil_cumlogit_cpo12,cil_cumlogit_cpo13,cil_cumlogit_cpo14,cil_cumlogit_cpo15,cil_cumlogit_cpo16,cil_cumlogit_cpo17,cil_cumlogit_cpo18,cil_cumlogit_cpo19,cil_cumlogit_cpo20,
           cil_cumlogit_cpo21,cil_cumlogit_cpo22,cil_cumlogit_cpo23,cil_cumlogit_cpo24,cil_cumlogit_cpo25,cil_cumlogit_cpo26,cil_cumlogit_cpo27,
           cil_cumlogit_lr1,cil_cumlogit_lr2,cil_cumlogit_lr3,cil_cumlogit_lr4,cil_cumlogit_lr5,cil_cumlogit_lr6,cil_cumlogit_lr7,cil_cumlogit_lr8,cil_cumlogit_lr9,cil_cumlogit_lr10,
           cil_cumlogit_lr11,cil_cumlogit_lr12,cil_cumlogit_lr13,cil_cumlogit_lr14,cil_cumlogit_lr15,cil_cumlogit_lr16,cil_cumlogit_lr17,cil_cumlogit_lr18,cil_cumlogit_lr19,
           cil_cumlogit_lr20,cil_cumlogit_lr21,cil_cumlogit_lr22,cil_cumlogit_lr23,cil_cumlogit_lr24,cil_cumlogit_lr25,cil_cumlogit_lr26,cil_cumlogit_lr27)
  
  uci <- c(ciu_cumlogit_po1,ciu_cumlogit_po2,ciu_cumlogit_po3,ciu_cumlogit_po4,ciu_cumlogit_po5,ciu_cumlogit_po6,ciu_cumlogit_po7,ciu_cumlogit_po8,ciu_cumlogit_po9,ciu_cumlogit_po10,
           ciu_cumlogit_po11,ciu_cumlogit_po12,ciu_cumlogit_po13,ciu_cumlogit_po14,ciu_cumlogit_po15,ciu_cumlogit_po16,ciu_cumlogit_po17,ciu_cumlogit_po18,ciu_cumlogit_po19,ciu_cumlogit_po20,
           ciu_cumlogit_po21,ciu_cumlogit_po22,ciu_cumlogit_po23,ciu_cumlogit_po24,ciu_cumlogit_po25,ciu_cumlogit_po26,ciu_cumlogit_po27,
           ciu_cumlogit_ppo1,ciu_cumlogit_ppo2,ciu_cumlogit_ppo3,ciu_cumlogit_ppo4,ciu_cumlogit_ppo5,ciu_cumlogit_ppo6,ciu_cumlogit_ppo7,ciu_cumlogit_ppo8,ciu_cumlogit_ppo9,ciu_cumlogit_ppo10,
           ciu_cumlogit_ppo11,ciu_cumlogit_ppo12,ciu_cumlogit_ppo13,ciu_cumlogit_ppo14,ciu_cumlogit_ppo15,ciu_cumlogit_ppo16,ciu_cumlogit_ppo17,ciu_cumlogit_ppo18,ciu_cumlogit_ppo19,
           ciu_cumlogit_ppo20,ciu_cumlogit_ppo21,ciu_cumlogit_ppo22,ciu_cumlogit_ppo23,ciu_cumlogit_ppo24,ciu_cumlogit_ppo25,ciu_cumlogit_ppo26,ciu_cumlogit_ppo27,
           ciu_cumlogit_lcpo1,ciu_cumlogit_lcpo2,ciu_cumlogit_lcpo3,ciu_cumlogit_lcpo4,ciu_cumlogit_lcpo5,ciu_cumlogit_lcpo6,ciu_cumlogit_lcpo7,ciu_cumlogit_lcpo8,ciu_cumlogit_lcpo9,
           ciu_cumlogit_lcpo10,ciu_cumlogit_lcpo11,ciu_cumlogit_lcpo12,ciu_cumlogit_lcpo13,ciu_cumlogit_lcpo14,ciu_cumlogit_lcpo15,ciu_cumlogit_lcpo16,ciu_cumlogit_lcpo17,ciu_cumlogit_lcpo18,
           ciu_cumlogit_lcpo19,ciu_cumlogit_lcpo20,ciu_cumlogit_lcpo21,ciu_cumlogit_lcpo22,ciu_cumlogit_lcpo23,ciu_cumlogit_lcpo24,ciu_cumlogit_lcpo25,ciu_cumlogit_lcpo26,ciu_cumlogit_lcpo27,
           ciu_cumlogit_cpo1,ciu_cumlogit_cpo2,ciu_cumlogit_cpo3,ciu_cumlogit_cpo4,ciu_cumlogit_cpo5,ciu_cumlogit_cpo6,ciu_cumlogit_cpo7,ciu_cumlogit_cpo8,ciu_cumlogit_cpo9,ciu_cumlogit_cpo10,ciu_cumlogit_cpo11,
           ciu_cumlogit_cpo12,ciu_cumlogit_cpo13,ciu_cumlogit_cpo14,ciu_cumlogit_cpo15,ciu_cumlogit_cpo16,ciu_cumlogit_cpo17,ciu_cumlogit_cpo18,ciu_cumlogit_cpo19,ciu_cumlogit_cpo20,
           ciu_cumlogit_cpo21,ciu_cumlogit_cpo22,ciu_cumlogit_cpo23,ciu_cumlogit_cpo24,ciu_cumlogit_cpo25,ciu_cumlogit_cpo26,ciu_cumlogit_cpo27,
           ciu_cumlogit_lr1,ciu_cumlogit_lr2,ciu_cumlogit_lr3,ciu_cumlogit_lr4,ciu_cumlogit_lr5,ciu_cumlogit_lr6,ciu_cumlogit_lr7,ciu_cumlogit_lr8,ciu_cumlogit_lr9,ciu_cumlogit_lr10,
           ciu_cumlogit_lr11,ciu_cumlogit_lr12,ciu_cumlogit_lr13,ciu_cumlogit_lr14,ciu_cumlogit_lr15,ciu_cumlogit_lr16,ciu_cumlogit_lr17,ciu_cumlogit_lr18,ciu_cumlogit_lr19,
           ciu_cumlogit_lr20,ciu_cumlogit_lr21,ciu_cumlogit_lr22,ciu_cumlogit_lr23,ciu_cumlogit_lr24,ciu_cumlogit_lr25,ciu_cumlogit_lr26,ciu_cumlogit_lr27)
  
  Method <- c(rep("PO",27), rep("Unconstrained PPO",27), rep("Linear CPPO", 27), rep("CPPO (divergent OR)", 27), rep("LR", 27))
  
  cumlogit <- c(1:27,1:27,1:27,1:27,1:27)
  
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
    scale_x_continuous(breaks = seq(1, 27, by = 1)) + 
    scale_y_continuous(breaks = seq(-10, 20, by = 5))
  ggsave("casestudy_hosp.png",width = 12, height = 12)
  
  