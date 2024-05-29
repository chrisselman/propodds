library(readxl)

data <- read_excel('dataforgraph_extra.xlsx')

library(tidyverse)

attach(data)
library(ggplot2)
library(ggpubr)
library(ggh4x)

myTheme <- theme(legend.text = element_text(size = 10), 
                 legend.title = element_text(size = 12), 
                 legend.key.size = unit(1, 'cm'),
                 plot.title = element_text (size = 12, face="bold"),
                 axis.title=element_text(size=12),
                 axis.text=element_text(size=14),
                 strip.text.x = element_text(size = 14),
                 strip.text.y = element_text(size = 14))

## Data generating mechanism: U shape 
## BIAS 
## Rename some variables 
attach(data)
names(data)
names(data)[6] = "Bias"
names(data)[4] = "Model"
attach(data)
df <- data[data$truemod == "U-shape",]
names(df)[2] = "U-Shape"

df$Bias <- round(df$Bias,3)

df$sampsize <- "n = 10,000"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "Symmetric control probabilities"
df$category[df$category == 3] <- "3 categories"
df$category[df$category == 7] <- "7 categories"
df$category[df$category == 11] <- "11 categories"
df$category <- ordered(df$category,
                       levels = c("3 categories", "7 categories", "11 categories"),
                       labels = c("3 categories", "7 categories", "11 categories"))  
df$Model <- ordered(df$Model,
                    levels = c("CPPO (divergent OR)", "Linear CPPO", "LR", "Unconstrained PPO", "PO"),
                    labels = c("CPPO (divergent OR)", "Linear CPPO", "LR", "Unconstrained PPO", "PO"))  


ggplot(df, aes(x = cumlogit, y = Model, fill = Bias)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = Bias), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("coral2","white","cornflowerblue"),limits=c(-0.5, 0.5)) +
  ##facet_grid2(sampsize+category~controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme
ggsave("ushape_table_bias.png",width = 20, height = 15)



## Data generating mechanism: 
## COVERAGE  
## Rename some variables 
## Rename some variables 
attach(data)
names(data)
names(data)[6] = "Bias"
names(data)[4] = "Model"
attach(data)
df <- data[data$truemod == "U-shape",]
names(df)[2] = "U-Shape"
names(df)[8] = "Coverage"

df$Coverage <- round(df$Coverage,1)

df$sampsize <- "n = 10,000"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "Symmetric control probabilities"
df$category[df$category == 3] <- "3 categories"
df$category[df$category == 7] <- "7 categories"
df$category[df$category == 11] <- "11 categories"
df$category <- ordered(df$category,
                       levels = c("3 categories", "7 categories", "11 categories"),
                       labels = c("3 categories", "7 categories", "11 categories"))  

df$Model <- ordered(df$Model,
                    levels = c("CPPO (divergent OR)", "Linear CPPO", "LR", "Unconstrained PPO", "PO"),
                    labels = c("CPPO (divergent OR)", "Linear CPPO", "LR", "Unconstrained PPO", "PO")) 

ggplot(df, aes(x = cumlogit, y = Model, fill = Coverage)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = Coverage), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("coral3","white","lightblue"),limits=c(0, 100), values=c(0, 0.95, 1)) +
  #facet_grid2(sampsize+category~controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme
ggsave("ushape_table_coverage.png",width = 20, height = 15)


## MSE  
## Rename some variables 
## Rename some variables 
attach(data)
names(data)
names(data)[6] = "Bias"
names(data)[4] = "Model"
attach(data)
df <- data[data$truemod == "U-shape",]
names(df)[2] = "U-Shape"
names(df)[10] = "MSE"

df$MSE <- round(df$MSE,3)

df$sampsize <- "n = 10,000"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "Symmetric control probabilities"
df$category[df$category == 3] <- "3 categories"
df$category[df$category == 7] <- "7 categories"
df$category[df$category == 11] <- "11 categories"
df$category <- ordered(df$category,
                       levels = c("3 categories", "7 categories", "11 categories"),
                       labels = c("3 categories", "7 categories", "11 categories"))  

df$Model <- ordered(df$Model,
                    levels = c("CPPO (divergent OR)", "Linear CPPO", "LR", "Unconstrained PPO", "PO"),
                    labels = c("CPPO (divergent OR)", "Linear CPPO", "LR", "Unconstrained PPO", "PO")) 

ggplot(df, aes(x = cumlogit, y = Model, fill = MSE)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = MSE), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.2)) +
 # facet_grid2(sampsize+category~controlprob) +
  labs(x = "Cumulative logit") +
  myTheme +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10))
ggsave("ushape_table_mse.png",width = 20, height = 15)



## Data generating mechanism: zero prob cat   
## BIAS 
## Rename some variables 
attach(data)
names(data)
names(data)[6] = "Bias"
names(data)[4] = "Model"
attach(data)
df <- data[data$controlprob == "Zero category skewed",]

df$Bias <- round(df$Bias,3)

df$sampsize <- "n = 10,000"
df$controlprob[df$controlprob == "Zero category skewed"] <- "Skewed control probability (middle close to zero)"
df$category[df$category == 3] <- "3 categories"
df$category[df$category == 7] <- "7 categories"
df$category[df$category == 11] <- "11 categories"
df$category <- ordered(df$category,
                       levels = c("3 categories", "7 categories", "11 categories"),
                       labels = c("3 categories", "7 categories", "11 categories"))  

df$Model <- ordered(df$Model,
                    levels = c("CPPO (divergent OR)", "Linear CPPO", "LR", "Unconstrained PPO", "PO"),
                    labels = c("CPPO (divergent OR)", "Linear CPPO", "LR", "Unconstrained PPO", "PO")) 
df$effectsize <- ordered(df$effectsize,
                         levels = c("Small [OR = 1.10]","Moderate [OR = 1.50]"),
                         labels = c("Small [OR = 1.10]","Moderate [OR = 1.50]"))  

ggplot(df, aes(x = cumlogit, y = Model, fill = Bias)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = Bias), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("coral2","white","cornflowerblue"),limits=c(-0.5, 0.5)) +
  facet_grid2(~truemod) +
  labs(x = "Cumulative logit") +
  myTheme + 
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10))
ggsave("zero_table_bias.png",width = 25, height = 10)



## Data generating mechanism: PO 
## COVERAGE  
## Rename some variables 
attach(data)
names(data)
names(data)[6] = "Bias"
names(data)[4] = "Model"
attach(data)
df <- data[data$controlprob == "Zero category skewed",]
names(df)[8] = "Coverage"

df$Coverage <- round(df$Coverage,1)


df$sampsize <- "n = 10,000"
df$controlprob[df$controlprob == "Zero category skewed"] <- "Skewed control probability (middle close to zero)"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "Symmetric control probabilities"
df$category[df$category == 3] <- "3 categories"
df$category[df$category == 7] <- "7 categories"
df$category[df$category == 11] <- "11 categories"
df$category <- ordered(df$category,
                       levels = c("3 categories", "7 categories", "11 categories"),
                       labels = c("3 categories", "7 categories", "11 categories"))  

df$Model <- ordered(df$Model,
                    levels = c("CPPO (divergent OR)", "Linear CPPO", "LR", "Unconstrained PPO", "PO"),
                    labels = c("CPPO (divergent OR)", "Linear CPPO", "LR", "Unconstrained PPO", "PO"))
df$effectsize <- ordered(df$effectsize,
                         levels = c("Small [OR = 1.10]","Moderate [OR = 1.50]"),
                         labels = c("Small [OR = 1.10]","Moderate [OR = 1.50]"))  

ggplot(df, aes(x = cumlogit, y = Model, fill = Coverage)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = Coverage), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("coral3","white","lightblue"),limits=c(0, 100), values=c(0, 0.95, 1)) +
  facet_grid2(~truemod) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme
ggsave("zero_table_coverage.png",width = 25, height = 10)



## Data generating mechanism: PO 
## MSE  
## Rename some variables 
attach(data)
names(data)
names(data)[6] = "Bias"
names(data)[4] = "Model"
attach(data)
df <- data[data$controlprob == "Zero category skewed",]
names(df)[10] = "MSE"

df$MSE <- round(df$MSE,3)


df$sampsize <- "n = 10,000"
df$controlprob[df$controlprob == "Zero category skewed"] <- "Skewed control probability (middle close to zero)"
df$controlprob[df$controlprob == "Symmetric"] <- "Symmetric control probabilities"
df$category[df$category == 3] <- "3 categories"
df$category[df$category == 7] <- "7 categories"
df$category[df$category == 11] <- "11 categories"
df$category <- ordered(df$category,
                       levels = c("3 categories", "7 categories", "11 categories"),
                       labels = c("3 categories", "7 categories", "11 categories"))  

df$Model <- ordered(df$Model,
                    levels = c("CPPO (divergent OR)", "Linear CPPO", "LR", "Unconstrained PPO", "PO"),
                    labels = c("CPPO (divergent OR)", "Linear CPPO", "LR", "Unconstrained PPO", "PO"))
df$effectsize <- ordered(df$effectsize,
                         levels = c("Small [OR = 1.10]","Moderate [OR = 1.50]"),
                         labels = c("Small [OR = 1.10]","Moderate [OR = 1.50]"))  

ggplot(df, aes(x = cumlogit, y = Model, fill = MSE)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = MSE), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.2)) +
  facet_grid2(~truemod) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("zero_table_mse.png",width = 25, height = 10)


## Summarise MCSEs 
## Data generating mechanism:
## BIAS 
## Rename some variables 
attach(data)
names(data)
names(data)[7] = "Bias MCSE"
names(data)[4] = "Model"
attach(data)
df <- data[data$truemod == "U-shape",]
names(df)[2] = "U-Shape"

df$`Bias MCSE` <- round(df$`Bias MCSE`,3)

df$sampsize <- "n = 10,000"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "Symmetric control probabilities"
df$category[df$category == 3] <- "3 categories"
df$category[df$category == 7] <- "7 categories"
df$category[df$category == 11] <- "11 categories"
df$category <- ordered(df$category,
                       levels = c("3 categories", "7 categories", "11 categories"),
                       labels = c("3 categories", "7 categories", "11 categories"))  

df$Model <- ordered(df$Model,
                    levels = c("CPPO (divergent OR)", "Linear CPPO", "LR", "Unconstrained PPO", "PO"),
                    labels = c("CPPO (divergent OR)", "Linear CPPO", "LR", "Unconstrained PPO", "PO"))  


ggplot(df, aes(x = cumlogit, y = Model, fill = `Bias MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Bias MCSE`), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.05)) +
  #facet_grid2(sampsize+category~controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("ushape_table_bias_mcse.png",width = 20, height = 15)



## Data generating mechanism: 
## COVERAGE  
## Rename some variables 
df <- data[data$truemod == "U-shape",]
names(df)[2] = "U-Shape"
names(df)[9] = "Coverage MCSE"

df$`Coverage MCSE` <- round(df$`Coverage MCSE`,3)

df$sampsize <- "n = 10,000"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "Symmetric control probabilities"
df$category[df$category == 3] <- "3 categories"
df$category[df$category == 7] <- "7 categories"
df$category[df$category == 11] <- "11 categories"
df$category <- ordered(df$category,
                       levels = c("3 categories", "7 categories", "11 categories"),
                       labels = c("3 categories", "7 categories", "11 categories"))  

df$Model <- ordered(df$Model,
                    levels = c("CPPO (divergent OR)", "Linear CPPO", "LR", "Unconstrained PPO", "PO"),
                    labels = c("CPPO (divergent OR)", "Linear CPPO", "LR", "Unconstrained PPO", "PO"))  


ggplot(df, aes(x = cumlogit, y = Model, fill = `Coverage MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Coverage MCSE`), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.05)) +
 # facet_grid2(sampsize+category~controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("ushape_table_coverage_mcse.png",width = 20, height = 15)



## Data generating mechanism: 
## MSE  
## Rename some variables 
df <- data[data$truemod == "U-shape",]
names(df)[2] = "U-Shape"
names(df)[11] = "MSE MCSE"

df$`MSE MCSE` <- round(df$`MSE MCSE`,3)

df$sampsize <- "n = 10,000"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "Symmetric control probabilities"
df$category[df$category == 3] <- "3 categories"
df$category[df$category == 7] <- "7 categories"
df$category[df$category == 11] <- "11 categories"
df$category <- ordered(df$category,
                       levels = c("3 categories", "7 categories", "11 categories"),
                       labels = c("3 categories", "7 categories", "11 categories"))  

df$Model <- ordered(df$Model,
                    levels = c("CPPO (divergent OR)", "Linear CPPO", "LR", "Unconstrained PPO", "PO"),
                    labels = c("CPPO (divergent OR)", "Linear CPPO", "LR", "Unconstrained PPO", "PO"))  


ggplot(df, aes(x = cumlogit, y = Model, fill = `MSE MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `MSE MCSE`), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.05)) +
 # facet_grid2(sampsize+category~controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("ushape_table_mse_mcse.png",width = 20, height = 15)



## Data generating mechanism
## BIAS 
## Rename some variables 
attach(data)
names(data)
names(data)[7] = "Bias MCSE"
names(data)[4] = "Model"
attach(data)

df <- data[data$controlprob == "Zero category skewed",]
df$sampsize <- "n = 10,000"
df$controlprob[df$controlprob == "Zero category skewed"] <- "Skewed control probability (middle close to zero)"
df$category[df$category == 3] <- "3 categories"
df$category[df$category == 7] <- "7 categories"
df$category[df$category == 11] <- "11 categories"
df$category <- ordered(df$category,
                       levels = c("3 categories", "7 categories", "11 categories"),
                       labels = c("3 categories", "7 categories", "11 categories"))  

df$Model <- ordered(df$Model,
                    levels = c("CPPO (divergent OR)", "Linear CPPO", "LR", "Unconstrained PPO", "PO"),
                    labels = c("CPPO (divergent OR)", "Linear CPPO", "LR", "Unconstrained PPO", "PO"))  

df$effectsize <- ordered(df$effectsize,
                         levels = c("Small [OR = 1.10]","Moderate [OR = 1.50]"),
                         labels = c("Small [OR = 1.10]","Moderate [OR = 1.50]"))  

df$`Bias MCSE` <- round(df$`Bias MCSE`,3)

ggplot(df, aes(x = cumlogit, y = Model, fill = `Bias MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Bias MCSE`), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.05)) +
  facet_grid2(~truemod) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("zero_table_bias_mcse.png",width = 25, height = 10)



## Data generating mechanism:
## COVERAGE  
## Rename some variables 
names(df)[9] = "Coverage MCSE"


df <- data[data$controlprob == "Zero category skewed",]
df$sampsize <- "n = 10,000"
df$controlprob[df$controlprob == "Zero category skewed"] <- "Skewed control probability (middle close to zero)"
df$category[df$category == 3] <- "3 categories"
df$category[df$category == 7] <- "7 categories"
df$category[df$category == 11] <- "11 categories"
df$category <- ordered(df$category,
                       levels = c("3 categories", "7 categories", "11 categories"),
                       labels = c("3 categories", "7 categories", "11 categories"))  

df$Model <- ordered(df$Model,
                    levels = c("CPPO (divergent OR)", "Linear CPPO", "LR", "Unconstrained PPO", "PO"),
                    labels = c("CPPO (divergent OR)", "Linear CPPO", "LR", "Unconstrained PPO", "PO"))  

df$effectsize <- ordered(df$effectsize,
                         levels = c("Small [OR = 1.10]","Moderate [OR = 1.50]"),
                         labels = c("Small [OR = 1.10]","Moderate [OR = 1.50]"))  

df$`Coverage MCSE` <- round(df$`Coverage MCSE`,3)

ggplot(df, aes(x = cumlogit, y = Model, fill = `Coverage MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Coverage MCSE`), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.05)) +
  facet_grid2(~truemod) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("zero_table_coverage_mcse.png",width = 25, height = 10)



## Data generating mechanism: 
## MSE  
## Rename some variables 
names(df)[11] = "MSE MCSE"

df <- data[data$controlprob == "Zero category skewed",]
df$sampsize <- "n = 10,000"
df$controlprob[df$controlprob == "Zero category skewed"] <- "Skewed control probability (middle close to zero)"

df$category[df$category == 3] <- "3 categories"
df$category[df$category == 7] <- "7 categories"
df$category[df$category == 11] <- "11 categories"
df$category <- ordered(df$category,
                       levels = c("3 categories", "7 categories", "11 categories"),
                       labels = c("3 categories", "7 categories", "11 categories"))  

df$Model <- ordered(df$Model,
                    levels = c("CPPO (divergent OR)", "Linear CPPO", "LR", "Unconstrained PPO", "PO"),
                    labels = c("CPPO (divergent OR)", "Linear CPPO", "LR", "Unconstrained PPO", "PO"))  

df$effectsize <- ordered(df$effectsize,
                         levels = c("Small [OR = 1.10]","Moderate [OR = 1.50]"),
                         labels = c("Small [OR = 1.10]","Moderate [OR = 1.50]"))  

df$`MSE MCSE` <- round(df$`MSE MCSE`,3)

ggplot(df, aes(x = cumlogit, y = Model, fill = `MSE MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `MSE MCSE`), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.05)) +
  facet_grid2(~truemod) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("zero_table_mse_mcse.png",width = 25, height = 10)
