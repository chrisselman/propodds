library(readxl)

data <- read_excel('dataforgraph.xlsx')

library(tidyverse)

attach(data)
library(ggplot2)
library(ggpubr)
library(ggh4x)

# Sample size of n = 10,000 
data <- subset(data,sampsize==10000)
myTheme <- theme(legend.text = element_text(size = 10), 
                 legend.title = element_text(size = 12), 
                 legend.key.size = unit(1, 'cm'),
                 plot.title = element_text (size = 12, face="bold"),
                 axis.title=element_text(size=12),
                 axis.text=element_text(size=14),
                 strip.text.x = element_text(size = 14),
                 strip.text.y = element_text(size = 14))

## Data generating mechanism: Linear CPPO 
## BIAS 
## Rename some variables 
attach(data)
names(data)
names(data)[6] = "Bias"
names(data)[4] = "Model"
attach(data)
df <- data[data$truemod == "Linear",]
names(df)[2] = "Linear"

df$Bias <- round(df$Bias,3)

df$sampsize <- "n = 1500"
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
  facet_grid2(sampsize+category~controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme
ggsave("linear_table_bias_10000.png",width = 20, height = 15)



## Data generating mechanism: Linear CPPO 
## COVERAGE  
## Rename some variables 
df <- data[data$truemod == "Linear",]
names(df)[8] = "Coverage"

df$Coverage <- round(df$Coverage,1)

df$sampsize <- "n = 1500"
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
  facet_grid2(sampsize+category~controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme
ggsave("linear_table_coverage_10000.png",width = 20, height = 15)



## Data generating mechanism: Linear CPPO 
## MSE  
## Rename some variables 
df <- data[data$truemod == "Linear",]
names(df)[10] = "MSE"

df$MSE <- round(df$MSE,3)

df$sampsize <- "n = 1500"
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
  facet_grid2(sampsize+category~controlprob) +
  labs(x = "Cumulative logit") +
  myTheme +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10))
ggsave("linear_table_mse_10000.png",width = 20, height = 15)


## Data generating mechanism: Linear CPPO 
## Relative bias  
## Rename some variables 
df <- data[data$truemod == "Linear",]
names(df)[14] = "Relative Bias (%)"

df$`Relative Bias (%)` <- round(df$`Relative Bias (%)`,1)

df$sampsize <- "n = 1500"
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

ggplot(df, aes(x = cumlogit, y = Model, fill = `Relative Bias (%)`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Relative Bias (%)`), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("coral2","white","cornflowerblue"),limits=c(-50, 50)) +
  facet_grid2(sampsize+category~controlprob) +
  labs(x = "Cumulative logit") +
  myTheme +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10))
ggsave("linear_table_relbias_10000.png",width = 20, height = 15)


## Data generating mechanism: PO  
## BIAS 
## Rename some variables 
attach(data)
names(data)
names(data)[6] = "Bias"
names(data)[4] = "Model"
attach(data)
df <- data[data$truemod == "Random PO [no violation]",]

df$Bias <- round(df$Bias,3)

df$sampsize <- "n = 1500"
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

ggplot(df, aes(x = cumlogit, y = Model, fill = Bias)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = Bias), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("coral2","white","cornflowerblue"),limits=c(-0.5, 0.5)) +
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  myTheme + 
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10))
ggsave("po_table_bias_10000.png",width = 25, height = 10)



## Data generating mechanism: PO 
## COVERAGE  
## Rename some variables 
attach(data)
names(data)
names(data)[6] = "Bias"
names(data)[4] = "Model"
attach(data)
df <- data[data$truemod == "Random PO [no violation]",]
names(df)[8] = "Coverage"

df$Coverage <- round(df$Coverage,1)

df$sampsize <- "n = 1500"
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
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme
ggsave("po_table_coverage_10000.png",width = 25, height = 10)



## Data generating mechanism: PO 
## MSE  
## Rename some variables 
attach(data)
names(data)
names(data)[6] = "Bias"
names(data)[4] = "Model"
attach(data)
df <- data[data$truemod == "Random PO [no violation]",]
names(df)[10] = "MSE"

df$MSE <- round(df$MSE,3)

df$sampsize <- "n = 1500"
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

ggplot(df, aes(x = cumlogit, y = Model, fill = MSE)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = MSE), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.2)) +
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("po_table_mse_10000.png",width = 25, height = 10)


## Relative bias  
## Rename some variables 
attach(data)
names(data)
names(data)[6] = "Bias"
names(data)[4] = "Model"
attach(data)
df <- data[data$truemod == "Random PO [no violation]",]
names(df)[14] = "Relative Bias (%)"

df$`Relative Bias (%)` <- round(df$`Relative Bias (%)`,1)

df$sampsize <- "n = 1500"
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

ggplot(df, aes(x = cumlogit, y = Model, fill = `Relative Bias (%)`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Relative Bias (%)`), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("coral2","white","cornflowerblue"),limits=c(-50, 50)) +
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  myTheme +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10))
ggsave("po_table_relbias_10000.png",width = 25, height = 10)


## Data generating mechanism: PO slight vio   
## BIAS 
## Rename some variables 
attach(data)
names(data)
names(data)[6] = "Bias"
names(data)[4] = "Model"
attach(data)
df <- data[data$truemod == "Random PO [slight violation]",]

df$Bias <- round(df$Bias,3)

df$sampsize <- "n = 1500"
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

ggplot(df, aes(x = cumlogit, y = Model, fill = Bias)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = Bias), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("coral2","white","cornflowerblue"),limits=c(-0.5, 0.5)) +
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme
ggsave("po_slightvio_table_bias_10000.png",width = 25, height = 10)



## Data generating mechanism: PO 
## COVERAGE  
## Rename some variables 
attach(data)
names(data)
names(data)[6] = "Bias"
names(data)[4] = "Model"
attach(data)
df <- data[data$truemod == "Random PO [slight violation]",]
names(df)[8] = "Coverage"

df$Coverage <- round(df$Coverage,1)

df$sampsize <- "n = 1500"
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
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("po_slightvio_table_coverage_10000.png",width = 25, height = 10)



## Data generating mechanism: PO 
## MSE  
## Rename some variables 
attach(data)
names(data)
names(data)[6] = "Bias"
names(data)[4] = "Model"
attach(data)
df <- data[data$truemod == "Random PO [slight violation]",]
names(df)[10] = "MSE"

df$MSE <- round(df$MSE,3)

df$sampsize <- "n = 1500"
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
ggplot(df, aes(x = cumlogit, y = Model, fill = MSE)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = MSE), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.2)) +
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("po_slightvio_table_mse_10000.png",width = 25, height = 10)


## Relative bias  
## Rename some variables 
attach(data)
names(data)
names(data)[6] = "Bias"
names(data)[4] = "Model"
attach(data)
df <- data[data$truemod == "Random PO [slight violation]",]
names(df)[14] = "Relative Bias (%)"

df$`Relative Bias (%)` <- round(df$`Relative Bias (%)`,1)

df$sampsize <- "n = 1500"
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

ggplot(df, aes(x = cumlogit, y = Model, fill = `Relative Bias (%)`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Relative Bias (%)`), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("coral2","white","cornflowerblue"),limits=c(-50, 50)) +
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  myTheme +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10))
ggsave("po_slightvio_table_relbias_10000.png",width = 25, height = 10)



## Data generating mechanism: PO moderate vio   
## BIAS 
## Rename some variables 
attach(data)
names(data)
names(data)[6] = "Bias"
names(data)[4] = "Model"
attach(data)
df <- data[data$truemod == "Random PO [moderate violation]",]

df$Bias <- round(df$Bias,3)

df$sampsize <- "n = 1500"
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

ggplot(df, aes(x = cumlogit, y = Model, fill = Bias)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = Bias), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("coral2","white","cornflowerblue"),limits=c(-0.5, 0.5)) +
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("po_moderatevio_table_bias_10000.png",width = 25, height = 10)



## Data generating mechanism: PO 
## COVERAGE  
## Rename some variables 
attach(data)
names(data)
names(data)[6] = "Bias"
names(data)[4] = "Model"
attach(data)
df <- data[data$truemod == "Random PO [moderate violation]",]
names(df)[8] = "Coverage"

df$Coverage <- round(df$Coverage,1)

df$sampsize <- "n = 1500"
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
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("po_moderatevio_table_coverage_10000.png",width = 25, height = 10)



## Data generating mechanism: PO 
## MSE  
## Rename some variables 
attach(data)
names(data)
names(data)[6] = "Bias"
names(data)[4] = "Model"
attach(data)
df <- data[data$truemod == "Random PO [moderate violation]",]
names(df)[10] = "MSE"

df$MSE <- round(df$MSE,3)

df$sampsize <- "n = 1500"
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

ggplot(df, aes(x = cumlogit, y = Model, fill = MSE)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = MSE), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.2)) +
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("po_moderatevio_table_mse_10000.png",width = 25, height = 10)

## Relative bias  
## Rename some variables 
attach(data)
names(data)
names(data)[6] = "Bias"
names(data)[4] = "Model"
attach(data)
df <- data[data$truemod == "Random PO [moderate violation]",]
names(df)[14] = "Relative Bias (%)"

df$`Relative Bias (%)` <- round(df$`Relative Bias (%)`,1)

df$sampsize <- "n = 1500"
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

ggplot(df, aes(x = cumlogit, y = Model, fill = `Relative Bias (%)`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Relative Bias (%)`), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("coral2","white","cornflowerblue"),limits=c(-50, 50)) +
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  myTheme +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10))
ggsave("po_moderatevio_table_relbias_10000.png",width = 25, height = 10)




## Data generating mechanism: divergent PO   
## BIAS 
## Rename some variables 
attach(data)
names(data)
names(data)[6] = "Bias"
names(data)[4] = "Model"
attach(data)
df <- data[data$truemod == "Divergent",]

df$Bias <- round(df$Bias,3)

df$sampsize <- "n = 1500"
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
ggplot(df, aes(x = cumlogit, y = Model, fill = Bias)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = Bias), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("coral2","white","cornflowerblue"),limits=c(-0.5, 0.5)) +
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("divergent_table_bias_10000.png",width = 25, height = 10)



## Data generating mechanism: Divergent 
## COVERAGE  
## Rename some variables 
attach(data)
names(data)
names(data)[6] = "Bias"
names(data)[4] = "Model"
attach(data)
df <- data[data$truemod == "Divergent",]
names(df)[8] = "Coverage"

df$Coverage <- round(df$Coverage,1)

df$sampsize <- "n = 1500"
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
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme
ggsave("divergent_table_coverage_10000.png",width = 25, height = 10)



## Data generating mechanism: Divergent 
## MSE  
## Rename some variables 
attach(data)
names(data)
names(data)[6] = "Bias"
names(data)[4] = "Model"
attach(data)
df <- data[data$truemod == "Divergent",]
names(df)[10] = "MSE"

df$MSE <- round(df$MSE,3)

df$sampsize <- "n = 1500"
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
ggplot(df, aes(x = cumlogit, y = Model, fill = MSE)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = MSE), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.2)) +
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("divergent_table_mse_10000.png",width = 25, height = 10)


## Relative bias  
## Rename some variables 
attach(data)
names(data)
names(data)[6] = "Bias"
names(data)[4] = "Model"
attach(data)
df <- data[data$truemod == "Divergent",]
names(df)[14] = "Relative Bias (%)"

df$`Relative Bias (%)` <- round(df$`Relative Bias (%)`,1)

df$sampsize <- "n = 1500"
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

ggplot(df, aes(x = cumlogit, y = Model, fill = `Relative Bias (%)`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Relative Bias (%)`), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("coral2","white","cornflowerblue"),limits=c(-50, 50)) +
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  myTheme +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10))
ggsave("divergent_table_relbias_10000.png",width = 25, height = 10)


## Summarise MCSEs 
## Data generating mechanism: Linear CPPO 
## BIAS 
## Rename some variables 
attach(data)
names(data)
names(data)[7] = "Bias MCSE"
names(data)[4] = "Model"
attach(data)
df <- data[data$truemod == "Linear",]
names(df)[2] = "Linear"

df$`Bias MCSE` <- round(df$`Bias MCSE`,3)

df$sampsize <- "n = 1500"
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
  facet_grid2(sampsize+category~controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("linear_table_bias_mcse_10000.png",width = 20, height = 15)



## Data generating mechanism: Linear CPPO 
## COVERAGE  
## Rename some variables 
df <- data[data$truemod == "Linear",]
names(df)[9] = "Coverage MCSE"

df$`Coverage MCSE` <- round(df$`Coverage MCSE`,3)

df$sampsize <- "n = 1500"
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
  facet_grid2(sampsize+category~controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("linear_table_coverage_mcse_10000.png",width = 20, height = 15)



## Data generating mechanism: Linear CPPO 
## MSE  
## Rename some variables 
df <- data[data$truemod == "Linear",]
names(df)[11] = "MSE MCSE"

df$`MSE MCSE` <- round(df$`MSE MCSE`,3)

df$sampsize <- "n = 1500"
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
  facet_grid2(sampsize+category~controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("linear_table_mse_mcse_10000.png",width = 20, height = 15)


## Data generating mechanism: Linear CPPO 
## MSE  
## Rename some variables 
df <- data[data$truemod == "Linear",]
names(df)[15] = "Relative Bias MCSE"

df$`Relative Bias MCSE` <- round(df$`Relative Bias MCSE`,3)

df$sampsize <- "n = 1500"
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


ggplot(df, aes(x = cumlogit, y = Model, fill = `Relative Bias MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Relative Bias MCSE`), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.05)) +
  facet_grid2(sampsize+category~controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("linear_table_relbias_mcse_10000.png",width = 20, height = 15)


## Data generating mechanism: Random PO 
## BIAS 
## Rename some variables 
attach(data)
names(data)
names(data)[7] = "Bias MCSE"
names(data)[4] = "Model"
attach(data)
df <- data[data$truemod == "Random PO [no violation]",]

df$`Bias MCSE` <- round(df$`Bias MCSE`,3)

df$sampsize <- "n = 1500"
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
ggplot(df, aes(x = cumlogit, y = Model, fill = `Bias MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Bias MCSE`), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.05)) +
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("po_table_bias_mcse_10000.png",width = 25, height = 10)



## Data generating mechanism:
## COVERAGE  
## Rename some variables 
df <- data[data$truemod == "Random PO [no violation]",]
names(df)[9] = "Coverage MCSE"

df$`Coverage MCSE` <- round(df$`Coverage MCSE`,3)

df$sampsize <- "n = 1500"
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
ggplot(df, aes(x = cumlogit, y = Model, fill = `Coverage MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Coverage MCSE`), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.05)) +
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("po_table_coverage_mcse_10000.png",width = 25, height = 10)



## Data generating mechanism: 
## MSE  
## Rename some variables 
df <- data[data$truemod == "Random PO [no violation]",]
names(df)[11] = "MSE MCSE"

df$`MSE MCSE` <- round(df$`MSE MCSE`,3)

df$sampsize <- "n = 1500"
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
ggplot(df, aes(x = cumlogit, y = Model, fill = `MSE MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `MSE MCSE`), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.05)) +
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("po_table_mse_mcse_10000.png",width = 25, height = 10)


## Data generating mechanism: 
## Rel bias  
## Rename some variables 
df <- data[data$truemod == "Random PO [no violation]",]
names(df)[15] = "Relative Bias MCSE"

df$`Relative Bias MCSE` <- round(df$`Relative Bias MCSE`,3)

df$sampsize <- "n = 1500"
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
ggplot(df, aes(x = cumlogit, y = Model, fill = `Relative Bias MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Relative Bias MCSE`), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.05)) +
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("po_table_relbias_mcse_10000.png",width = 25, height = 10)


## Data generating mechanism: Random PO 
## BIAS 
## Rename some variables 
attach(data)
names(data)
names(data)[7] = "Bias MCSE"
names(data)[4] = "Model"
attach(data)
df <- data[data$truemod == "Random PO [slight violation]",]

df$`Bias MCSE` <- round(df$`Bias MCSE`,3)

df$sampsize <- "n = 1500"
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
ggplot(df, aes(x = cumlogit, y = Model, fill = `Bias MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Bias MCSE`), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.05)) +
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("po_slightvio_table_bias_mcse_10000.png",width = 25, height = 10)



## Data generating mechanism:
## COVERAGE  
## Rename some variables 
df <- data[data$truemod == "Random PO [slight violation]",]
names(df)[9] = "Coverage MCSE"

df$`Coverage MCSE` <- round(df$`Coverage MCSE`,3)

df$sampsize <- "n = 1500"
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

ggplot(df, aes(x = cumlogit, y = Model, fill = `Coverage MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Coverage MCSE`), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.05)) +
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("po_slightvio_table_coverage_mcse_10000.png",width = 25, height = 10)



## Data generating mechanism: 
## MSE  
## Rename some variables 
df <- data[data$truemod == "Random PO [slight violation]",]
names(df)[11] = "MSE MCSE"

df$`MSE MCSE` <- round(df$`MSE MCSE`,3)

df$sampsize <- "n = 1500"
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
ggplot(df, aes(x = cumlogit, y = Model, fill = `MSE MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `MSE MCSE`), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.05)) +
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("po_slightvio_table_mse_mcse_10000.png",width = 25, height = 10)


## Rel bias  
## Rename some variables 
df <- data[data$truemod == "Random PO [slight violation]",]
names(df)[15] = "Relative Bias MCSE"

df$`Relative Bias MCSE` <- round(df$`Relative Bias MCSE`,3)

df$sampsize <- "n = 1500"
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
ggplot(df, aes(x = cumlogit, y = Model, fill = `Relative Bias MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Relative Bias MCSE`), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.05)) +
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("po_slightvio_table_relbias_mcse_10000.png",width = 25, height = 10)


## Data generating mechanism: Random PO 
## BIAS 
## Rename some variables 
attach(data)
names(data)
names(data)[7] = "Bias MCSE"
names(data)[4] = "Model"
attach(data)
df <- data[data$truemod == "Random PO [moderate violation]",]

df$`Bias MCSE` <- round(df$`Bias MCSE`,3)

df$sampsize <- "n = 1500"
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
ggplot(df, aes(x = cumlogit, y = Model, fill = `Bias MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Bias MCSE`), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.05)) +
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("po_moderatevio_table_bias_mcse_10000.png",width = 25, height = 10)



## Data generating mechanism:
## COVERAGE  
## Rename some variables 
df <- data[data$truemod == "Random PO [moderate violation]",]
names(df)[9] = "Coverage MCSE"

df$`Coverage MCSE` <- round(df$`Coverage MCSE`,3)

df$sampsize <- "n = 1500"
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
ggplot(df, aes(x = cumlogit, y = Model, fill = `Coverage MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Coverage MCSE`), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.05)) +
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("po_moderatevio_table_coverage_mcse_10000.png",width = 25, height = 10)



## Data generating mechanism: 
## MSE  
## Rename some variables 
df <- data[data$truemod == "Random PO [moderate violation]",]
names(df)[11] = "MSE MCSE"

df$`MSE MCSE` <- round(df$`MSE MCSE`,3)

df$sampsize <- "n = 1500"
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
ggplot(df, aes(x = cumlogit, y = Model, fill = `MSE MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `MSE MCSE`), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.05)) +
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("po_moderatevio_table_mse_mcse_10000.png",width = 25, height = 10)


## Rel bias  
## Rename some variables 
df <- data[data$truemod == "Random PO [moderate violation]",]
names(df)[15] = "Relative Bias MCSE"

df$`Relative Bias MCSE` <- round(df$`Relative Bias MCSE`,3)

df$sampsize <- "n = 1500"
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
ggplot(df, aes(x = cumlogit, y = Model, fill = `Relative Bias MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Relative Bias MCSE`), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.05)) +
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("po_moderatevio_table_relbias_mcse_10000.png",width = 25, height = 10)


## Data generating mechanism: divergent 
## BIAS 
## Rename some variables 
attach(data)
names(data)
names(data)[7] = "Bias MCSE"
names(data)[4] = "Model"
attach(data)
df <- data[data$truemod == "Divergent",]

df$`Bias MCSE` <- round(df$`Bias MCSE`,3)

df$sampsize <- "n = 1500"
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
ggplot(df, aes(x = cumlogit, y = Model, fill = `Bias MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Bias MCSE`), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.05)) +
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("divergent_table_bias_mcse_10000.png",width = 25, height = 10)



## Data generating mechanism:
## COVERAGE  
## Rename some variables 
df <- data[data$truemod == "Divergent",]
names(df)[9] = "Coverage MCSE"

df$`Coverage MCSE` <- round(df$`Coverage MCSE`,3)

df$sampsize <- "n = 1500"
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
ggplot(df, aes(x = cumlogit, y = Model, fill = `Coverage MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Coverage MCSE`), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.05)) +
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("divergent_table_coverage_mcse_10000.png",width = 25, height = 10)



## Data generating mechanism: 
## MSE  
## Rename some variables 
df <- data[data$truemod == "Divergent",]
names(df)[11] = "MSE MCSE"

df$`MSE MCSE` <- round(df$`MSE MCSE`,3)

df$sampsize <- "n = 1500"
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


ggplot(df, aes(x = cumlogit, y = Model, fill = `MSE MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `MSE MCSE`), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.05)) +
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("divergent_table_mse_mcse_10000.png",limitsize = F,width = 25, height = 10)


## Rel bias  
## Rename some variables 
df <- data[data$truemod == "Divergent",]
names(df)[15] = "Relative Bias MCSE"

df$`Relative Bias MCSE` <- round(df$`Relative Bias MCSE`,3)

df$sampsize <- "n = 1500"
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
ggplot(df, aes(x = cumlogit, y = Model, fill = `Relative Bias MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Relative Bias MCSE`), color = "black", size = 3.5) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.05)) +
  facet_grid2(sampsize+category~effectsize+controlprob) +
  labs(x = "Cumulative logit") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  myTheme 
ggsave("divergent_table_relbias_mcse_10000.png",width = 25, height = 10)




