
library(readxl)

data <- read_excel('dataforgraph.xlsx')

library(tidyverse)

attach(data)
library(ggplot2)
library(ggpubr)

myTheme <- theme(legend.text = element_text(size = 10), 
                 legend.title = element_text(size = 10), 
                 legend.key.size = unit(0.75, 'cm'),
                 plot.title = element_text (size = 14, face="bold"),
                 axis.title=element_text(size=12),
                 axis.text=element_text(size=12))
names(data)[4] = "Model"
attach(data)

## BIAS 
## TO DO: split these graphs by sample size when simulations are done 

## Divergent - 11 categories 
df<- subset(data, category ==11 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Divergent")
  a <- ggplot(df,aes(cumlogit, bias, group = Model,color = Model)) + 
  geom_point() +
  geom_line() + 
  coord_cartesian(xlim=c(1, 10), ylim=c(-0.4, 0.15)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(-0.4, -0.3, -0.2, -0.1, 0, 0.1)) 
  
  df2<- subset(data, category == 11 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Divergent")
  b <- ggplot(df2,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(-0.4, 0.15)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(-0.4, -0.3, -0.2, -0.1, 0, 0.1))

  df3<- subset(data, category ==11 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Divergent")
  c <- ggplot(df3,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(-0.4, 0.15)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(-0.4, -0.3, -0.2, -0.1, 0, 0.1))
  
  
  df4<- subset(data, category == 11 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Divergent")
  d <- ggplot(df4,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(-0.4, 0.15)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(-0.4, -0.3, -0.2, -0.1, 0, 0.1))
  
  ggarrange(a, b, c, d,
            labels = c("A", "B", "C", "D"),
            ncol = 2, nrow = 2,
            common.legend = TRUE, legend = "bottom")
  ggsave("diverg_11cat_bias.png",width = 12, height = 12)
  
  ## Divergent - 7 categories 
  
  df<- subset(data, category ==7 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Divergent")
  a <- ggplot(df,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(-0.4, 0.2)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(-0.4, -0.3, -0.2, -0.1, 0, 0.1,0.2)) 
  
  df2<- subset(data, category == 7 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Divergent")
  b <- ggplot(df2,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(-0.4, 0.2)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(-0.4, -0.3, -0.2, -0.1, 0, 0.1,0.2)) 
  
  df3<- subset(data, category ==7 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Divergent")
  c <- ggplot(df3,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(-0.4, 0.2)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(-0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2))
  
  
  df4<- subset(data, category == 7 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Divergent")
  d <- ggplot(df4,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(-0.4, 0.2)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(-0.4, -0.3, -0.2, -0.1, 0, 0.1,0.2))
  
  ggarrange(a, b, c, d,
            labels = c("A", "B", "C", "D"),
            ncol = 2, nrow = 2,
            common.legend = TRUE, legend = "bottom")
  ggsave("diverg_7cat_bias.png",width = 12, height = 12)
  

## Divergent - 3 categories 
  df<- subset(data, category ==3 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Divergent")
  a <- ggplot(df,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(-0.4, 0.2)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(-0.4, -0.3, -0.2, -0.1, 0, 0.1,0.2)) 
  
  df2<- subset(data, category == 3 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Divergent")
  b <- ggplot(df2,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(-0.4, 0.2)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(-0.4, -0.3, -0.2, -0.1, 0, 0.1,0.2)) 
  
  df3<- subset(data, category ==3 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Divergent")
  c <- ggplot(df3,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(-0.4, 0.2)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(-0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2))
  
  
  df4<- subset(data, category == 3 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Divergent")
  d <- ggplot(df4,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(-0.4, 0.2)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(-0.4, -0.3, -0.2, -0.1, 0, 0.1,0.2))
  
  ggarrange(a, b, c, d,
            labels = c("A", "B", "C", "D"),
            ncol = 2, nrow = 2,
            common.legend = TRUE, legend = "bottom")
  ggsave("diverg_3cat_bias.png",width = 12, height = 12)
  
  
  
## Linear 
 ## 11 categories 
  df<- subset(data, category ==11 & controlprob == "Skewed" & truemod == "Linear" & effectsize == "0.06 increase")
  a <- ggplot(df,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(-0.3, 0.4)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4)) 

  df2<- subset(data, category == 11 & controlprob == "Symmetric" & truemod == "Linear" & effectsize == "0.06 increase")
  b <- ggplot(df2,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(-0.3, 0.4)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4))
  
  ggarrange(a, b, 
            labels = c("A", "B"),
            ncol = 2, nrow = 1,
            common.legend = TRUE, legend = "bottom")
  ggsave("linear_11cat_bias.png",width = 12, height = 12)
  
  
  
  ## 7 categories 
  df<- subset(data, category ==7 & controlprob == "Skewed" & truemod == "Linear" & effectsize == "0.06 increase")
  a <- ggplot(df,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(-0.2, 0.2)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4)) 
  
  df2<- subset(data, category == 7 & controlprob == "Symmetric" & truemod == "Linear" & effectsize == "0.06 increase")
  b <- ggplot(df2,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(-0.2, 0.2)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4))

  ggarrange(a, b,
            labels = c("A", "B"),
            ncol = 2, nrow = 1,
            common.legend = TRUE, legend = "bottom")
  ggsave("linear_7cat_bias.png",width = 12, height = 12)
  
  
  
  ## 3 categories 
  df<- subset(data, category ==3 & controlprob == "Skewed" & truemod == "Linear" & effectsize == "0.06 increase")
  a <- ggplot(df,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(-0.2, 0.2)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4)) 
  
  df2<- subset(data, category == 3 & controlprob == "Symmetric" & truemod == "Linear" & effectsize == "0.06 increase")
  b <- ggplot(df2,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(-0.2, 0.2)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4))
  
  ggarrange(a, b, 
            labels = c("A", "B"),
            ncol = 2, nrow = 1,
            common.legend = TRUE, legend = "bottom")
  ggsave("linear_3cat_bias.png",width = 12, height = 12)
  
  
## Random PO - 11 categories 
  myTheme <- theme(legend.text = element_text(size = 9), 
                   legend.title = element_text(size = 8), 
                   legend.key.size = unit(1, 'cm'),
                   plot.title = element_text (size = 8, face="bold"),
                   axis.title=element_text(size=6))
  
  df<- subset(data, category ==11 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [no violation]")
  a <- ggplot(df,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(-0.1, 0.1)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(-0.1, -0.05, 0, 0.05, 0.1)) 
  
  df2<- subset(data, category == 11 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [no violation]")
  b <- ggplot(df2,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(-0.1, 0.1)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(-0.1, -0.05, 0, 0.05, 0.1))
  
  df3<- subset(data, category ==11 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [no violation]")
  c <- ggplot(df3,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(-0.1, 0.1)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(-0.1, -0.05, 0, 0.05, 0.1))
  
  
  df4<- subset(data, category == 11 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [no violation]")
  d <- ggplot(df4,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(-0.1, 0.1)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(-0.1, -0.05, 0, 0.05, 0.1))
  
  df5<- subset(data, category ==11 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [slight violation]")
  e <- ggplot(df5,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(-0.1, 0.1)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(-0.1, -0.05, 0, 0.05, 0.1)) 
  
  df6<- subset(data, category == 11 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [slight violation]")
  f <- ggplot(df6,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(-0.1, 0.1)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(-0.1, -0.05, 0, 0.05, 0.1))
  
  df7<- subset(data, category ==11 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [slight violation]")
  g <- ggplot(df7,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(-0.1, 0.1)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(-0.1, -0.05, 0, 0.05, 0.1))
  
  
  df8<- subset(data, category == 11 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [slight violation]")
  h <- ggplot(df8,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(-0.1, 0.1)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(-0.1, -0.05, 0, 0.05, 0.1))
  
  
  
  df9<- subset(data, category ==11 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [moderate violation]")
  i <- ggplot(df9,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(-0.1, 0.1)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(-0.1, -0.05, 0, 0.05, 0.1)) 
  
  df10<- subset(data, category == 11 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [moderate violation]")
  j <- ggplot(df10,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(-0.1, 0.1)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(-0.1, -0.05, 0, 0.05, 0.1))
  
  df11<- subset(data, category ==11 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [moderate violation]")
  k <- ggplot(df11,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(-0.1, 0.1)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(-0.1, -0.05, 0, 0.05, 0.1))
  
  
  df12<- subset(data, category == 11 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [moderate violation]")
  l <- ggplot(df12,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(-0.1, 0.1)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(-0.1, -0.05, 0, 0.05, 0.1))
  
  
  x<-ggarrange(a, b, e,f, i, j, c, d, g, h, k, l,
            labels = c("A", "B", "C", "D", "E", "F","G","H", "I", "J", "K", "L"),
            ncol = 6, nrow = 2,
            common.legend = TRUE, legend = "bottom")
  ggsave("randompo_11cat_bias.png", plot=x, limitsize = F,width = 25, height = 12)
  
  
  
  ## Random PO - 7 categories 
  myTheme <- theme(legend.text = element_text(size = 9), 
                   legend.title = element_text(size = 8), 
                   legend.key.size = unit(1, 'cm'),
                   plot.title = element_text (size = 8, face="bold"),
                   axis.title=element_text(size=6))
  
  df<- subset(data, category ==7 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [no violation]")
  a <- ggplot(df,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(-0.05, 0.025)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(-0.05, -0.025, 0, 0.025)) 
  
  df2<- subset(data, category == 7 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [no violation]")
  b <- ggplot(df2,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(-0.05, 0.025)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(-0.05, -0.025, 0, 0.025))
  
  df3<- subset(data, category ==7 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [no violation]")
  c <- ggplot(df3,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(-0.05, 0.025)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(-0.05, -0.025, 0, 0.025))
  
  
  df4<- subset(data, category == 7 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [no violation]")
  d <- ggplot(df4,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(-0.05, 0.025)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(-0.05, -0.025, 0, 0.025))
  
  df5<- subset(data, category ==7 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [slight violation]")
  e <- ggplot(df5,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(-0.05, 0.025)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(-0.05, -0.025, 0, 0.025)) 
  
  df6<- subset(data, category == 7 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [slight violation]")
  f <- ggplot(df6,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(-0.05, 0.025)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(-0.05, -0.025, 0, 0.025))
  
  df7<- subset(data, category ==7 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [slight violation]")
  g <- ggplot(df7,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(-0.05, 0.025)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(-0.05, -0.025, 0, 0.025))
  
  
  df8<- subset(data, category == 7 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [slight violation]")
  h <- ggplot(df8,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(-0.05, 0.025)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(-0.05, -0.025, 0, 0.025))
  
  
  
  df9<- subset(data, category ==7 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [moderate violation]")
  i <- ggplot(df9,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(-0.05, 0.025)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(-0.05, -0.025, 0, 0.025)) 
  
  df10<- subset(data, category == 7 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [moderate violation]")
  j <- ggplot(df10,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(-0.05, 0.025)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(-0.05, -0.025, 0, 0.025))
  
  df11<- subset(data, category ==7 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [moderate violation]")
  k <- ggplot(df11,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(-0.05, 0.025)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(-0.05, -0.025, 0, 0.025))
  
  
  df12<- subset(data, category == 7 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [moderate violation]")
  l <- ggplot(df12,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(-0.05, 0.025)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(-0.05, -0.025, 0, 0.025))
  
  
  x<-ggarrange(a, b, e,f, i, j, c, d, g, h, k, l,
               labels = c("A", "B", "C", "D", "E", "F","G","H", "I", "J", "K", "L"),
               ncol = 6, nrow = 2,
               common.legend = TRUE, legend = "bottom")
  ggsave("randompo_7cat_bias.png", plot=x, limitsize = F,width = 25, height = 12)
  
  
  
  
  ## Random PO - 3 categories 
  myTheme <- theme(legend.text = element_text(size = 9), 
                   legend.title = element_text(size = 8), 
                   legend.key.size = unit(1, 'cm'),
                   plot.title = element_text (size = 8, face="bold"),
                   axis.title=element_text(size=6))
  
  df<- subset(data, category ==3 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [no violation]")
  a <- ggplot(df,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(-0.01, 0.01)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(-0.01, -0.005, 0, 0.005, 0.01)) 
  
  df2<- subset(data, category == 3 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [no violation]")
  b <- ggplot(df2,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(-0.01, 0.01)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(-0.01, -0.005, 0, 0.005, 0.01))
  
  df3<- subset(data, category ==3 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [no violation]")
  c <- ggplot(df3,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(-0.01, 0.01)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(-0.01, -0.005, 0, 0.005, 0.01))
  
  
  df4<- subset(data, category == 3 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [no violation]")
  d <- ggplot(df4,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(-0.01, 0.01)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(-0.01, -0.005, 0, 0.005, 0.01))
  
  df5<- subset(data, category ==3 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [slight violation]")
  e <- ggplot(df5,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(-0.01, 0.01)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(-0.01, -0.005, 0, 0.005, 0.01)) 
  
  df6<- subset(data, category == 3 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [slight violation]")
  f <- ggplot(df6,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(-0.01, 0.01)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(-0.01, -0.005, 0, 0.005, 0.01))
  
  df7<- subset(data, category ==3 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [slight violation]")
  g <- ggplot(df7,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(-0.01, 0.01)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(-0.01, -0.005, 0, 0.005, 0.01))
  
  
  df8<- subset(data, category == 3 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [slight violation]")
  h <- ggplot(df8,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(-0.01, 0.01)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(-0.01, -0.005, 0, 0.005, 0.01))
  
  
  
  df9<- subset(data, category ==3 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [moderate violation]")
  i <- ggplot(df9,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(-0.01, 0.01)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(-0.01, -0.005, 0, 0.005, 0.01)) 
  
  df10<- subset(data, category == 3 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [moderate violation]")
  j <- ggplot(df10,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(-0.01, 0.01)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(-0.01, -0.005, 0, 0.005, 0.01))
  
  df11<- subset(data, category ==3 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [moderate violation]")
  k <- ggplot(df11,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(-0.01, 0.01)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(-0.01, -0.005, 0, 0.005, 0.01))
  
  
  df12<- subset(data, category == 3 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [moderate violation]")
  l <- ggplot(df12,aes(cumlogit, bias, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(-0.01, 0.01)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Bias") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(-0.01, -0.005, 0, 0.005, 0.01))
  
  
  x<-ggarrange(a, b, e,f, i, j, c, d, g, h, k, l,
               labels = c("A", "B", "C", "D", "E", "F","G","H", "I", "J", "K", "L"),
               ncol = 6, nrow = 2,
               common.legend = TRUE, legend = "bottom")
  ggsave("randompo_3cat_bias.png", plot=x, limitsize = F,width = 25, height = 12)
  

# COVERAGE 
  myTheme <- theme(legend.text = element_text(size = 10), 
                   legend.title = element_text(size = 10), 
                   legend.key.size = unit(0.75, 'cm'),
                   plot.title = element_text (size = 14, face="bold"),
                   axis.title=element_text(size=12))
  ## TO DO: split these graphs by sample size when simulations are done 
  
  ## Divergent - 11 categories 
  df<- subset(data, category ==11 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Divergent")
  a <- ggplot(df,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(0, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100)) 
  
  df2<- subset(data, category == 11 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Divergent")
  b <- ggplot(df2,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(0, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100))
  
  df3<- subset(data, category ==11 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Divergent")
  c <- ggplot(df3,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(0, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100))
  
  
  df4<- subset(data, category == 11 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Divergent")
  d <- ggplot(df4,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(0, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100))
  
  ggarrange(a, b, c, d,
            labels = c("A", "B", "C", "D"),
            ncol = 2, nrow = 2,
            common.legend = TRUE, legend = "bottom")
  ggsave("diverg_11cat_coverage.png",width = 12, height = 12)
  
  ## Divergent - 7 categories 
  
  df<- subset(data, category ==7 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Divergent")
  a <- ggplot(df,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(0, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100)) 
  
  df2<- subset(data, category == 7 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Divergent")
  b <- ggplot(df2,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(0, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100))
  
  df3<- subset(data, category ==7 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Divergent")
  c <- ggplot(df3,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(0, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100))
  
  
  df4<- subset(data, category == 7 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Divergent")
  d <- ggplot(df4,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(0, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100))
  
  ggarrange(a, b, c, d,
            labels = c("A", "B", "C", "D"),
            ncol = 2, nrow = 2,
            common.legend = TRUE, legend = "bottom")
  ggsave("diverg_7cat_coverage.png",width = 12, height = 12)
  
  
  ## Divergent - 3 categories 
  df<- subset(data, category ==3 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Divergent")
  a <- ggplot(df,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(0, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100)) 
  
  df2<- subset(data, category == 3 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Divergent")
  b <- ggplot(df2,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(0, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100))
  
  df3<- subset(data, category ==3 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Divergent")
  c <- ggplot(df3,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(0, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100))
  
  
  df4<- subset(data, category == 3 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Divergent")
  d <- ggplot(df4,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(0, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100))
  
  ggarrange(a, b, c, d,
            labels = c("A", "B", "C", "D"),
            ncol = 2, nrow = 2,
            common.legend = TRUE, legend = "bottom")
  ggsave("diverg_3cat_coverage.png",width = 12, height = 12)
  
  
  
  ## Linear 
  ## 11 categories 
  df<- subset(data, category ==11 & controlprob == "Skewed" & truemod == "Linear" & effectsize == "0.06 increase")
  a <- ggplot(df,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(0, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100)) 
  
  df2<- subset(data, category == 11 & controlprob == "Symmetric" & truemod == "Linear" & effectsize == "0.06 increase")
  b <- ggplot(df2,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(0, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100))
  
  ggarrange(a, b, 
            labels = c("A", "B"),
            ncol = 2, nrow = 1,
            common.legend = TRUE, legend = "bottom")
  ggsave("linear_11cat_coverage.png",width = 12, height = 12)
  
  
  
  ## 7 categories 
  df<- subset(data, category ==7 & controlprob == "Skewed" & truemod == "Linear" & effectsize == "0.06 increase")
  a <- ggplot(df,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(0, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100)) 
  
  df2<- subset(data, category == 7 & controlprob == "Symmetric" & truemod == "Linear" & effectsize == "0.06 increase")
  b <- ggplot(df2,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(0, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100))
  
  ggarrange(a, b,
            labels = c("A", "B"),
            ncol = 2, nrow = 1,
            common.legend = TRUE, legend = "bottom")
  ggsave("linear_7cat_coverage.png",width = 12, height = 12)
  
  
  
  ## 3 categories 
  df<- subset(data, category ==3 & controlprob == "Skewed" & truemod == "Linear" & effectsize == "0.06 increase")
  a <- ggplot(df,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(0, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100)) 
  
  df2<- subset(data, category == 3 & controlprob == "Symmetric" & truemod == "Linear" & effectsize == "0.06 increase")
  b <- ggplot(df2,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(0, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100))
  
  ggarrange(a, b,
            labels = c("A", "B"),
            ncol = 2, nrow = 1,
            common.legend = TRUE, legend = "bottom")
  ggsave("linear_3cat_coverage.png",width = 12, height = 12)
  
  
  ## Random PO - 11 categories 
  myTheme <- theme(legend.text = element_text(size = 9), 
                   legend.title = element_text(size = 8), 
                   legend.key.size = unit(1, 'cm'),
                   plot.title = element_text (size = 8, face="bold"),
                   axis.title=element_text(size=6))
  
  df<- subset(data, category ==11 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [no violation]")
  a <- ggplot(df,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100)) 
  
  df2<- subset(data, category == 11 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [no violation]")
  b <- ggplot(df2,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100))
  
  df3<- subset(data, category ==11 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [no violation]")
  c <- ggplot(df3,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100))
  
  
  df4<- subset(data, category == 11 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [no violation]")
  d <- ggplot(df4,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100))
  
  df5<- subset(data, category ==11 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [slight violation]")
  e <- ggplot(df5,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100)) 
  
  df6<- subset(data, category == 11 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [slight violation]")
  f <- ggplot(df6,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100))
  
  df7<- subset(data, category ==11 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [slight violation]")
  g <- ggplot(df7,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100))
  
  
  df8<- subset(data, category == 11 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [slight violation]")
  h <- ggplot(df8,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100))
  
  
  
  df9<- subset(data, category ==11 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [moderate violation]")
  i <- ggplot(df9,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100)) 
  
  df10<- subset(data, category == 11 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [moderate violation]")
  j <- ggplot(df10,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100))
  
  df11<- subset(data, category ==11 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [moderate violation]")
  k <- ggplot(df11,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100))
  
  
  df12<- subset(data, category == 11 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [moderate violation]")
  l <- ggplot(df12,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100))
  
  
  x<-ggarrange(a, b, e,f, i, j, c, d, g, h, k, l,
               labels = c("A", "B", "C", "D", "E", "F","G","H", "I", "J", "K", "L"),
               ncol = 6, nrow = 2,
               common.legend = TRUE, legend = "bottom")
  ggsave("randompo_11cat_coverage.png", plot=x, limitsize = F,width = 25, height = 12)
  
  
  ## Random PO - 7 categories 
  myTheme <- theme(legend.text = element_text(size = 9), 
                   legend.title = element_text(size = 8), 
                   legend.key.size = unit(1, 'cm'),
                   plot.title = element_text (size = 8, face="bold"),
                   axis.title=element_text(size=6))
  
  df<- subset(data, category ==7 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [no violation]")
  a <- ggplot(df,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100)) 
  
  df2<- subset(data, category == 7 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [no violation]")
  b <- ggplot(df2,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100))
  
  df3<- subset(data, category ==7 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [no violation]")
  c <- ggplot(df3,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100))
  
  
  df4<- subset(data, category == 7 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [no violation]")
  d <- ggplot(df4,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100))
  
  df5<- subset(data, category ==7 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [slight violation]")
  e <- ggplot(df5,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100)) 
  
  df6<- subset(data, category == 7 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [slight violation]")
  f <- ggplot(df6,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100))
  
  df7<- subset(data, category ==7 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [slight violation]")
  g <- ggplot(df7,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100))
  
  
  df8<- subset(data, category == 7 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [slight violation]")
  h <- ggplot(df8,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100))
  
  
  
  df9<- subset(data, category ==7 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [moderate violation]")
  i <- ggplot(df9,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100)) 
  
  df10<- subset(data, category == 7 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [moderate violation]")
  j <- ggplot(df10,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100))
  
  df11<- subset(data, category ==7 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [moderate violation]")
  k <- ggplot(df11,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100))
  
  
  df12<- subset(data, category == 7 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [moderate violation]")
  l <- ggplot(df12,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100))
  
  
  x<-ggarrange(a, b, e,f, i, j, c, d, g, h, k, l,
               labels = c("A", "B", "C", "D", "E", "F","G","H", "I", "J", "K", "L"),
               ncol = 6, nrow = 2,
               common.legend = TRUE, legend = "bottom")
  ggsave("randompo_7cat_coverage.png", plot=x, limitsize = F,width = 25, height = 12)
  
  
  
  ## Random PO - 3 categories 
  myTheme <- theme(legend.text = element_text(size = 9), 
                   legend.title = element_text(size = 8), 
                   legend.key.size = unit(1, 'cm'),
                   plot.title = element_text (size = 8, face="bold"),
                   axis.title=element_text(size=6))
  
  df<- subset(data, category ==3 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [no violation]")
  a <- ggplot(df,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100)) 
  
  df2<- subset(data, category == 3 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [no violation]")
  b <- ggplot(df2,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100))
  
  df3<- subset(data, category ==3 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [no violation]")
  c <- ggplot(df3,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100))
  
  
  df4<- subset(data, category == 3 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [no violation]")
  d <- ggplot(df4,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100))
  
  df5<- subset(data, category ==3 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [slight violation]")
  e <- ggplot(df5,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100)) 
  
  df6<- subset(data, category == 3 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [slight violation]")
  f <- ggplot(df6,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100))
  
  df7<- subset(data, category ==3 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [slight violation]")
  g <- ggplot(df7,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100))
  
  
  df8<- subset(data, category == 3 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [slight violation]")
  h <- ggplot(df8,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100))
  
  
  
  df9<- subset(data, category ==3 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [moderate violation]")
  i <- ggplot(df9,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100)) 
  
  df10<- subset(data, category == 3 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [moderate violation]")
  j <- ggplot(df10,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100))
  
  df11<- subset(data, category ==3 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [moderate violation]")
  k <- ggplot(df11,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100))
  
  
  df12<- subset(data, category == 3 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [moderate violation]")
  l <- ggplot(df12,aes(cumlogit, coverage, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(80, 100)) +
    geom_hline(yintercept = 95, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "Coverage (%)") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(80, 85, 90, 95, 100))
  
  
  x<-ggarrange(a, b, e,f, i, j, c, d, g, h, k, l,
               labels = c("A", "B", "C", "D", "E", "F","G","H", "I", "J", "K", "L"),
               ncol = 6, nrow = 2,
               common.legend = TRUE, legend = "bottom")
  ggsave("randompo_3cat_coverage.png", plot=x, limitsize = F,width = 25, height = 12)
  
# MSE 
  ## TO DO: split these graphs by sample size when simulations are done 
  
  ## Divergent - 11 categories 
  df<- subset(data, category ==11 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Divergent")
  a <- ggplot(df,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(0, 0.15)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(0, 0.05, 0.10, 0.15)) 
  
  df2<- subset(data, category == 11 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Divergent")
  b <- ggplot(df2,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(0, 0.15)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(0, 0.05, 0.10, 0.15))
  
  df3<- subset(data, category ==11 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Divergent")
  c <- ggplot(df3,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(0, 0.15)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(0, 0.05, 0.10, 0.15))
  
  
  df4<- subset(data, category == 11 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Divergent")
  d <- ggplot(df4,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(0, 0.15)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(0, 0.05, 0.10, 0.15))
  
  ggarrange(a, b, c, d,
            labels = c("A", "B", "C", "D"),
            ncol = 2, nrow = 2,
            common.legend = TRUE, legend = "bottom")
  ggsave("diverg_11cat_mse.png",width = 12, height = 12)
  
  ## Divergent - 7 categories 
  
  df<- subset(data, category ==7 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Divergent")
  a <- ggplot(df,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(0, 0.20)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(0, 0.05, 0.10, 0.15,0.20)) 
  
  df2<- subset(data, category == 7 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Divergent")
  b <- ggplot(df2,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(0, 0.20)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(0, 0.05, 0.10, 0.15,0.20))
  
  df3<- subset(data, category ==7 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Divergent")
  c <- ggplot(df3,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(0, 0.20)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(0, 0.05, 0.10, 0.15,0.20))
  
  
  df4<- subset(data, category == 7 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Divergent")
  d <- ggplot(df4,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(0, 0.20)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(0, 0.05, 0.10, 0.15,0.20))
  
  ggarrange(a, b, c, d,
            labels = c("A", "B", "C", "D"),
            ncol = 2, nrow = 2,
            common.legend = TRUE, legend = "bottom")
  ggsave("diverg_7cat_mse.png",width = 12, height = 12)
  
  
  ## Divergent - 3 categories 
  df<- subset(data, category ==3 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Divergent")
  a <- ggplot(df,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(0, 0.05)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(0, 0.01, 0.02, 0.03, 0.04, 0.05)) 
  
  df2<- subset(data, category == 3 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Divergent")
  b <- ggplot(df2,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(0, 0.05)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(0, 0.01, 0.02, 0.03, 0.04, 0.05))
  
  df3<- subset(data, category ==3 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Divergent")
  c <- ggplot(df3,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(0, 0.05)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(0, 0.01, 0.02, 0.03, 0.04, 0.05))
  
  
  df4<- subset(data, category == 3 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Divergent")
  d <- ggplot(df4,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(0, 0.05)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(0, 0.01, 0.02, 0.03, 0.04, 0.05))
  
  ggarrange(a, b, c, d,
            labels = c("A", "B", "C", "D"),
            ncol = 2, nrow = 2,
            common.legend = TRUE, legend = "bottom")
  ggsave("diverg_3cat_mse.png",width = 12, height = 12)
  
  
  
  ## Linear 
  ## 11 categories 
  df<- subset(data, category ==11 & controlprob == "Skewed" & truemod == "Linear" & effectsize == "0.06 increase")
  a <- ggplot(df,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(0, 0.15)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(0, 0.05, 0.1, 0.15)) 
  
  df2<- subset(data, category == 11 & controlprob == "Symmetric" & truemod == "Linear" & effectsize == "0.06 increase")
  b <- ggplot(df2,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(0, 0.15)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(0, 0.05, 0.1, 0.15))
  
  ggarrange(a, b, 
            labels = c("A", "B"),
            ncol = 2, nrow = 1,
            common.legend = TRUE, legend = "bottom")
  ggsave("linear_11cat_mse.png",width = 12, height = 12)
  
  
  
  ## 7 categories 
  df<- subset(data, category ==7 & controlprob == "Skewed" & truemod == "Linear" & effectsize == "0.06 increase")
  a <- ggplot(df,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(0, 0.05)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(0, 0.01, 0.02, 0.03, 0.04, 0.05)) 
  
  df2<- subset(data, category == 7 & controlprob == "Symmetric" & truemod == "Linear" & effectsize == "0.06 increase")
  b <- ggplot(df2,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(0, 0.05)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(0, 0.01, 0.02, 0.03, 0.04, 0.05))
  
  ggarrange(a, b, 
            labels = c("A", "B"),
            ncol = 2, nrow = 1,
            common.legend = TRUE, legend = "bottom")
  ggsave("linear_7cat_mse.png",width = 12, height = 12)
  
  
  
  
  ## 3 categories 
  df<- subset(data, category ==3 & controlprob == "Skewed" & truemod == "Linear" & effectsize == "0.06 increase")
  a <- ggplot(df,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(0, 0.02)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(0, 0.01, 0.02)) 
  
  df2<- subset(data, category == 3 & controlprob == "Symmetric" & truemod == "Linear" & effectsize == "0.06 increase")
  b <- ggplot(df2,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(0, 0.02)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(0, 0.01, 0.02))
  
  ggarrange(a, b, 
            labels = c("A", "B"),
            ncol = 2, nrow = 1,
            common.legend = TRUE, legend = "bottom")
  ggsave("linear_3cat_mse.png",width = 12, height = 12)
  
  
  ## Random PO - 11 categories 
  myTheme <- theme(legend.text = element_text(size = 9), 
                   legend.title = element_text(size = 8), 
                   legend.key.size = unit(1, 'cm'),
                   plot.title = element_text (size = 8, face="bold"),
                   axis.title=element_text(size=6))
  
  df<- subset(data, category ==11 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [no violation]")
  a <- ggplot(df,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(0, 0.15)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(0, 0.05, 0.1, 0.15)) 
  
  df2<- subset(data, category == 11 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [no violation]")
  b <- ggplot(df2,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(0, 0.15)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(0, 0.05, 0.1, 0.15))
  
  df3<- subset(data, category ==11 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [no violation]")
  c <- ggplot(df3,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(0, 0.15)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(0, 0.05, 0.1, 0.15))
  
  
  df4<- subset(data, category == 11 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [no violation]")
  d <- ggplot(df4,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(0, 0.15)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(0, 0.05, 0.1, 0.15))
  
  df5<- subset(data, category ==11 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [slight violation]")
  e <- ggplot(df5,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(0, 0.15)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(0, 0.05, 0.1, 0.15)) 
  
  df6<- subset(data, category == 11 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [slight violation]")
  f <- ggplot(df6,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(0, 0.15)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(0, 0.05, 0.1, 0.15))
  
  df7<- subset(data, category ==11 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [slight violation]")
  g <- ggplot(df7,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(0, 0.15)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(0, 0.05, 0.1, 0.15))
  
  
  df8<- subset(data, category == 11 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [slight violation]")
  h <- ggplot(df8,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(0, 0.15)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(0, 0.05, 0.1, 0.15))
  
  
  
  df9<- subset(data, category ==11 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [moderate violation]")
  i <- ggplot(df9,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(0, 0.15)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(0, 0.05, 0.1, 0.15)) 
  
  df10<- subset(data, category == 11 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [moderate violation]")
  j <- ggplot(df10,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(0, 0.15)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(0, 0.05, 0.1, 0.15))
  
  df11<- subset(data, category ==11 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [moderate violation]")
  k <- ggplot(df11,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(0, 0.15)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(0, 0.05, 0.1, 0.15))
  
  
  df12<- subset(data, category == 11 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [moderate violation]")
  l <- ggplot(df12,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 10), ylim=c(0, 0.15)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks=c(0, 0.05, 0.1, 0.15))
  
  
  x<-ggarrange(a, b, e,f, i, j, c, d, g, h, k, l,
               labels = c("A", "B", "C", "D", "E", "F","G","H", "I", "J", "K", "L"),
               ncol = 6, nrow = 2,
               common.legend = TRUE, legend = "bottom")
  ggsave("randompo_11cat_mse.png", plot=x, limitsize = F,width = 25, height = 12)
  
  
  
  ## Random PO - 7 categories 
  myTheme <- theme(legend.text = element_text(size = 9), 
                   legend.title = element_text(size = 8), 
                   legend.key.size = unit(1, 'cm'),
                   plot.title = element_text (size = 8, face="bold"),
                   axis.title=element_text(size=6))
  
  df<- subset(data, category ==7 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [no violation]")
  a <- ggplot(df,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(0, 0.1)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(0, 0.02, 0.04, 0.06, 0.08, 0.1)) 
  
  df2<- subset(data, category == 7 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [no violation]")
  b <- ggplot(df2,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(0, 0.1)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(0, 0.02, 0.04, 0.06, 0.08, 0.1))
  
  df3<- subset(data, category ==7 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [no violation]")
  c <- ggplot(df3,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(0, 0.1)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(0, 0.02, 0.04, 0.06, 0.08, 0.1))
  
  
  df4<- subset(data, category == 7 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [no violation]")
  d <- ggplot(df4,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(0, 0.1)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(0, 0.02, 0.04, 0.06, 0.08, 0.1))
  
  df5<- subset(data, category ==7 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [slight violation]")
  e <- ggplot(df5,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(0, 0.1)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(0, 0.02, 0.04, 0.06, 0.08, 0.1)) 
  
  df6<- subset(data, category == 7 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [slight violation]")
  f <- ggplot(df6,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(0, 0.1)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(0, 0.02, 0.04, 0.06, 0.08, 0.1))
  
  df7<- subset(data, category ==7 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [slight violation]")
  g <- ggplot(df7,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(0, 0.1)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(0, 0.02, 0.04, 0.06, 0.08, 0.1))
  
  
  df8<- subset(data, category == 7 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [slight violation]")
  h <- ggplot(df8,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(0, 0.1)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(0, 0.02, 0.04, 0.06, 0.08, 0.1))
  
  
  
  df9<- subset(data, category ==7 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [moderate violation]")
  i <- ggplot(df9,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(0, 0.1)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(0, 0.02, 0.04, 0.06, 0.08, 0.1)) 
  
  df10<- subset(data, category == 7 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [moderate violation]")
  j <- ggplot(df10,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(0, 0.1)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(0, 0.02, 0.04, 0.06, 0.08, 0.1))
  
  df11<- subset(data, category ==7 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [moderate violation]")
  k <- ggplot(df11,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(0, 0.1)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(0, 0.02, 0.04, 0.06, 0.08, 0.1))
  
  
  df12<- subset(data, category == 7 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [moderate violation]")
  l <- ggplot(df12,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 6), ylim=c(0, 0.1)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(0, 0.02, 0.04, 0.06, 0.08, 0.1))
  
  
  x<-ggarrange(a, b, e,f, i, j, c, d, g, h, k, l,
               labels = c("A", "B", "C", "D", "E", "F","G","H", "I", "J", "K", "L"),
               ncol = 6, nrow = 2,
               common.legend = TRUE, legend = "bottom")
  ggsave("randompo_7cat_mse.png", plot=x, limitsize = F,width = 25, height = 12)
  
  
  
  
  
  ## Random PO - 3 categories 
  myTheme <- theme(legend.text = element_text(size = 9), 
                   legend.title = element_text(size = 8), 
                   legend.key.size = unit(1, 'cm'),
                   plot.title = element_text (size = 8, face="bold"),
                   axis.title=element_text(size=6))
  
  
  df<- subset(data, category ==3 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [no violation]")
  a <- ggplot(df,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(0, 0.05)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(0, 0.01, 0.02, 0.03, 0.04, 0.05)) 
  
  df2<- subset(data, category == 3 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [no violation]")
  b <- ggplot(df2,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(0, 0.05)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(0, 0.01, 0.02, 0.03, 0.04, 0.05))
  
  df3<- subset(data, category ==3 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [no violation]")
  c <- ggplot(df3,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(0, 0.05)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(0, 0.01, 0.02, 0.03, 0.04, 0.05))
  
  
  df4<- subset(data, category == 3 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [no violation]")
  d <- ggplot(df4,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(0, 0.05)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, no violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(0, 0.01, 0.02, 0.03, 0.04, 0.05))
  
  df5<- subset(data, category ==3 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [slight violation]")
  e <- ggplot(df5,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(0, 0.05)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(0, 0.01, 0.02, 0.03, 0.04, 0.05)) 
  
  df6<- subset(data, category == 3 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [slight violation]")
  f <- ggplot(df6,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(0, 0.05)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(0, 0.01, 0.02, 0.03, 0.04, 0.05))
  
  df7<- subset(data, category ==3 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [slight violation]")
  g <- ggplot(df7,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(0, 0.05)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(0, 0.01, 0.02, 0.03, 0.04, 0.05))
  
  
  df8<- subset(data, category == 3 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [slight violation]")
  h <- ggplot(df8,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(0, 0.05)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, slight violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(0, 0.01, 0.02, 0.03, 0.04, 0.05))
  
  
  
  df9<- subset(data, category ==3 & controlprob == "Skewed" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [moderate violation]")
  i <- ggplot(df9,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(0, 0.05)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and small effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(0, 0.01, 0.02, 0.03, 0.04, 0.05)) 
  
  df10<- subset(data, category == 3 & controlprob == "Symmetric" & effectsize == "Small [OR = 1.10]" & truemod == "Random PO [moderate violation]")
  j <- ggplot(df10,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(0, 0.05)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and small effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(0, 0.01, 0.02, 0.03, 0.04, 0.05))
  
  df11<- subset(data, category ==3 & controlprob == "Skewed" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [moderate violation]")
  k <- ggplot(df11,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(0, 0.05)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Skewed and moderate effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(0, 0.01, 0.02, 0.03, 0.04, 0.05))
  
  
  df12<- subset(data, category == 3 & controlprob == "Symmetric" & effectsize == "Moderate [OR = 1.50]" & truemod == "Random PO [moderate violation]")
  l <- ggplot(df12,aes(cumlogit, mse, group = Model,color = Model)) + 
    geom_point() +
    geom_line() + 
    coord_cartesian(xlim=c(1, 2), ylim=c(0, 0.05)) +
    geom_hline(yintercept = 0, linetype = "twodash") +
    myTheme +
    ggtitle ("Symmetric and moderate effect, moderate violation, n = 1500") +
    labs(x = "Cumulative logit", y = "MSE") +
    scale_x_continuous(breaks=c(1,2)) +
    scale_y_continuous(breaks=c(0, 0.01, 0.02, 0.03, 0.04, 0.05))
  
  
  x<-ggarrange(a, b, e,f, i, j, c, d, g, h, k, l,
               labels = c("A", "B", "C", "D", "E", "F","G","H", "I", "J", "K", "L"),
               ncol = 6, nrow = 2,
               common.legend = TRUE, legend = "bottom")
  ggsave("randompo_3cat_mse.png", plot=x, limitsize = F,width = 25, height = 12)
  
  
  
