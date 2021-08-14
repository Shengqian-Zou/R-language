# Assignment 1 part2 
# Name: Shengqian Zou   Student ID: z5203970
library(GGally)
library(MVN)
library(readr)
library(ICS)
library(ICSNP)

# Question 1
datas<-read_csv('abalone3M.csv')
mvn(datas)
ggpairs(datas)

#Question 2
datas_new<- datas
datas_new['Whole weight']<-datas['Whole weight']^(1/3)
datas_new['Shucked weight']<-datas['Shucked weight']^(1/3)
datas_new['Viscera weight']<-datas['Viscera weight']^(1/3)
datas_new['Shell weight']<-datas['Shell weight']^(1/3)
ggpairs(datas_new)
mvn(datas_new)
sum(datas_new)

#Quetion 3
n <- nrow(datas) # sample size
p <- 3 # number of variables
m <- colMeans(datas[1:3]) # sample mean
m
m1 <- m[1:3]
S <- cov(datas[1:3]) # S
Sinv <- solve(S) # S^-1
mu <- c(110,85,30)
Tsquare <- n*t(m1-mu)%*%Sinv%*%(m1-mu)
Tsquare

G <- p*(n-1)/(n-p)*qf(0.95,p,n-p)
G

Pvalue<-1-pf(Tsquare*(n-p)/(p*(n-1)),p,n-p)
Pvalue
#datas
HotellingsT2(X=datas[1:3], mu=c(110,85,30))
Tsquare/(p*(n-1)/(n-p))
Tsquare


