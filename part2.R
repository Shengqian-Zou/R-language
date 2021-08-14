library(CCA)
library(readr)
library(CCP)
datas<-read_csv('C:/Users/M/Desktop/5855/ass2/price-cons.csv')
#Q1   No
#Q2 
#If use data with physical units to do linear regression, you can clearly find the physical meaning of the change of the independent variable on the dependent variable. The standardized data cannot directly reflect this change, and it is not convenient for data analysis. 

#Q3
U = datas[,2:3] 
V = datas[,4:5] 

CC <- cc(U,V)
Q3_1 <- CC$cor
Q3_1
Q3_2_U <- CC$xcoef[, 1]
Q3_2_U
Q3_2_V <- CC$ycoef[, 1]
Q3_2_V
p.perm(U,V, type = "Wilks")

#Q4 No

#Q5
n <- nrow(datas)
p <- ncol(U)
q <- ncol(V)
p.asym(CC$cor, n, p, q, tstat = "Wilks")

#Q6
library(ggm)
R_raw<-datas[,2:5]
Sigma<-cor(R_raw)
#c1
(r34.12 <- pcor(c(3,4,1:2), Sigma))
#c2
pcor.test(r34.12, 2, 20)
#c3
datas.mlm <- lm(cbind(beef_consumption ,pork_consumption)~steer_price +hog_price, data=datas)

