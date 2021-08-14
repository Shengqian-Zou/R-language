# Assignment 1 part2 
# Name: Shengqian Zou   Student ID: z5203970
library(ggplot2)
library(GGally)
library(MVN)
library(readr)
library(mvtnorm)
library(ICS)
library(ICSNP)

# Question 1
abalone <- read_csv('abalone3M.csv')
mvn(abalone)
ggpairs(abalone)

#Question 2
abalone_new <- abalone
abalone_new['Whole weight'] <- abalone_new['Whole weight']^(1/3)
abalone_new['Shucked weight'] <- abalone_new['Shucked weight']^(1/3)
abalone_new['Viscera weight'] <- abalone_new['Viscera weight']^(1/3)
abalone_new['Shell weight'] <- abalone_new['Shell weight']^(1/3)
ggpairs(abalone_new)
mvn(abalone_new)
sum(abalone_new) # = hint2

#Quetion 3
n <- nrow(abalone) 
p <- ncol(abalone[1:3]) 
m <- colMeans(abalone[1:3]) 
S <- cov(abalone[1:3]) 
Sinv <- solve(S) 
mu <- c(110,85,30)
(T_square <- n*t(m-mu)%*%Sinv%*%(m-mu))
(G <- p*(n-1)/(n-p)*qf(0.95,p,n-p))
(P_value<-1-pf(T_square*(n-p)/(p*(n-1)),p,n-p))
(T_square/(p*(n-1)/(n-p))) # =T.2
HotellingsT2(X=abalone[1:3], mu=c(110,85,30))

# Hypothesis_0:mu = c(110,85,30) if P_value ≥ α 
# Hypothesis_1:mu ≠ c(110,85,30) if P_value < α 
# Because P_value=0.01754 < α=0.05, so we can know that Hypothesis_0 is False
# and Hypothesis_1 is True.
# So we choose A.There is sufficient evidence to conclude that population means 
# of lengths, diameters, and heights of male abalone are different from 110 mm, 
# 85 mm, and 30 mm respectively.

