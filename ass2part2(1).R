set.seed(0)
price_c <- runif(20,1,2)
price_p <- runif(20,0,0.2) + price_c

con_c <- rnorm(20,0,2) + 50 - 10 * price_c
con_p <- 50 - 8 * price_p - 0.25 * con_c + rnorm(20,0,0.11)


#con_c: consumption coca cola 可口可乐销量(百瓶)
#con_p: consumption pepsi 百事可乐销量(百瓶)
#price_c. price_p: 相对的价格(dollar)
data <- data.frame(con_c, con_p, price_c, price_p)
#read.csv() read.table()来import data

#A
#未经标准化的
library(CCA)
U = data[,1:2] #第1,2列 con_c,con_p
V = data[,3:4] #第4,5列 price_c price_p

CC <- cc(U,V)
CC$cor #[1] 0.9990433 0.3566381
CC$xcoef #U
#         [,1]       [,2]
#con_c -0.1196187 -0.4489827
#con_p -0.4084557  0.7230999

cx1 <- CC$xcoef[, 1]

CC$ycoef #V
#         [,1]        [,2]
#price_c 0.4585528  17.65593
#price_p 3.0043256 -17.60170

#-0.1196187 和 -0.4084557百瓶相应的可乐销量变化
#概率上对应
#0.4585528 和 3.0043256左右的价格dollar增长
cy1 <- CC$ycoef[, 1]


 
#需不需要标准化
#标准化
Us <- scale(U)
Vs <- scale(V)

CC <- cc(Us,Vs)
CC$cor #[1] 0.9990433 0.3566381
CC$xcoef #U
#         [,1]       [,2]
#con_c -0.3680884 -1.381601
#con_p -0.7032104  1.244912
cx1s <- CC$xcoef[, 1]

CC$ycoef
#         [,1]        [,2]
#price_c 0.1339765  5.158575
#price_p 0.8682261 -5.086751

#不再有单位概念
#-0.3680884 和 -0.732104销量比率的下降
#概率上伴随
#0.1339765 和 0.8682261 增幅
cy1s <- CC$ycoef[, 1]


#需不需要标准化
#无所谓
#no 8 yes 1 开
#个人倾向no

#因为我们在知道单位的情况下
#且单位同意(百瓶和dollar)
#可以直观感受互相直接的联系
#研究dollar和consumption百瓶的关系


#如果单位不知道
#或者不同意(比如说:百事可乐是dollar，可口可乐是rmb)
#那么倾向于正则化(standardise)

#标准化之后主要是看pepsi price 和 coca cola price和之前的占比
#(不是我们关心的话题)
#我现在想知道pepsi 和 coca price哪个对consumption相加影响大
#这个时候肯定要正则化




#B
#无论标准不标准化
#cor 一致
#[1] 0.9990433 0.3566381

cx1
cy1

#-0.1196187 和 -0.4084557百瓶相应的可乐销量变化
#概率上对应
#0.4585528 和 3.0043256左右的价格dollar增长


library(CCP)
p.perm(X,Y, type = "wilks")
#可以是任何别的test
#Permutation resampling using wilks's statistic:
#stat0     mstat nboot   nexcess     p
#0.001669166  0.7954156    999     0 0

#pvalue 0                         (这里)
#permutation test 没有1to2的分类


#H_0: consumption 和 price之间不存在线性相关
#H_1: H_0在放屁


#C
cor(data) #两可乐销量正比
#这一般是经济原因
#经济好价格低两都卖的多
cov(data)

#直觉上应该买一种就不买另一种
#substitute effect
#把价格的影响去除掉
#condition掉
Sigma <- cov(data)

#conditional convariance
Sigmacond <- Sigma[1:2,1:2] - Sigma[1:2,3:4] %*%solve(Sigma[3:4,3:4]) %*% Sigma[3:4,1:2]

#负相关
#应该找出次conditional cov对应的conditional cor

RHOCOND <- diag(1/sqrt(diag(Sigmacond))) %*% Sigmacond %*% diag(1/sqrt(diag(Sigmacond)))

library(ggm)
pcor.test(RHOCOND[1,2], 2, 20)
#H_0: no fucking partial correlation

#$pvalue
#[1] 5.991941e-11

#H_0 fuck off 条件后的cor显著

#q=2
#the number of variables in the conditioning set.
n = nrow(data) #data 几行


#MLM
lm(cbind(con_c,con_p)~price_c+price_p, data = data)
#cbind(con_c,con_p) Y 被预测的 二维的
#price_c+price_p X feature

#Y_1(即con_c) = beta10+beta11*X_1(即price_c)+beta12*X_2(即price_p)
#Y_2(即con_p) = beta20+beta21*X_1(即price_c)+beta22*X_2(即price_p)

#Coefficients:
#               con_c   con_p
#(Intercept)   48.984  37.795
#price_c      -10.757   2.029
#price_p        1.459  -7.776

#price_c 每下降 1 dollar con_c 增加10.757百瓶销量
#price_p 每上升 1 dollar con_c 增加 1.459百瓶销量




