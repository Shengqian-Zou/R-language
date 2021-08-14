Sigma <- matrix(c(1, 0.8, 0.2, 0.8, 0.9, 0.4, 0.2, 0.4, 1.9), 3, 3)
Mu <-  matrix(c(-0.1, -0.9, -0.2, 0.3, -0.4, -0.6, -1.6, 0, 0.6), 3, 3, byrow=TRUE, dimnames = list(c("Normal", "Anxiety", "Obsession"), c("A", "B", "C")))

############## a(测试) ######################
coefficient <- matrix(nrow = 3, ncol = 3)
intercept <- c()
Sinv<-solve(Sigma)

for(i in 1:3){
  cat('Group',i,'\n')
  coefficient[i,] <- Mu[i,]%*%Sinv
  cat('coefficient:', coefficient[i,],'\n')
  intercept[i] <- -1/2*Mu[i,]%*%Sinv%*%Mu[i,]
  cat('Intercept', intercept[i],'\n','\n')
}

############# a i ######################

ans_a_i_N <- function(A,B,C) {
  coefficient <- Mu[1,]%*%Sinv
  intercept <- -1/2*Mu[1,]%*%Sinv%*%Mu[1,]
  output <- coefficient%*%c(A,B,C)+intercept+log(1/3)
  return (output)
}

ans_a_i_A <- function(A,B,C) {
  coefficient<-Mu[2,]%*%Sinv
  intercept<- -1/2*Mu[2,]%*%Sinv%*%Mu[2,]
  output<-coefficient%*%c(A,B,C)+intercept+log(1/3)
  
  return (output)
}
ans_a_i_O <- function(A,B,C) {
  coefficient <- Mu[3,]%*%Sinv
  intercept <- -1/2*Mu[3,]%*%Sinv%*%Mu[3,]
  output <- coefficient%*%c(A,B,C)+intercept+log(1/3)
  return (output)
}



############### a ii ####################
#A=−0.5 , B=−0.4, C=−0.4 c(-0.5, -0.4, -0.4)
ans_a_i_N(-0.5, -0.4, -0.4)
ans_a_i_A(-0.5, -0.4, -0.4)
ans_a_i_O(-0.5, -0.4, -0.4)
P = exp(-2.553894) + exp(-2.070091) +exp(-4.702368)
ans_ai = exp(-2.553894)/P
ans_aii = exp(-2.070091)/P
ans_aiii = exp(-4.702368)/P


############### a iii#####################
ans_a_i_N_p <- function(A,B,C) {
  coefficient <- Mu[1,]%*%Sinv
  intercept <- -1/2*Mu[1,]%*%Sinv%*%Mu[1,]
  output <- coefficient%*%c(A,B,C)+intercept+log(0.4)
  return (output)
}

ans_a_i_A_p <- function(A,B,C) {
  coefficient<-Mu[2,]%*%Sinv
  intercept<- -1/2*Mu[2,]%*%Sinv%*%Mu[2,]
  output<-coefficient%*%c(A,B,C)+intercept+log(0.3)
  
  return (output)
}
ans_a_i_O_p <- function(A,B,C) {
  coefficient <- Mu[3,]%*%Sinv
  intercept <- -1/2*Mu[3,]%*%Sinv%*%Mu[3,]
  output <- coefficient%*%c(A,B,C)+intercept+log(0.3)
  return (output)
}

ans_a_i_N_p(-0.5, -0.4, -0.4)
ans_a_i_A_p(-0.5, -0.4, -0.4)
ans_a_i_O_p(-0.5, -0.4, -0.4)

P_2 = exp(-2.371572) + exp(-2.175452) +exp(-4.807729)
(ans_ai_p = exp(-2.371572)/P_2)
(ans_aii_p = exp(-2.175452)/P_2)
(ans_aiii_p = exp(-4.807729)/P_2)
############## a iv ######################
Delta<-sqrt((Mu[2,]-Mu[3,])%*%solve(Sigma)%*%(Mu[2,]-Mu[3,]))
(ans_a_iv_pAO <-pnorm(-Delta/2))
Delta_OA<-sqrt((Mu[3,]-Mu[2,])%*%solve(Sigma)%*%(Mu[3,]-Mu[2,]))
(ans_a_iv_pOA <-pnorm(-Delta_OA/2))



################### b i ###################
G <- c('Normal', 'Anxiety', 'Obsession', 'Normal', 'Anxiety', 'Normal', 'Normal', 'Obsession', 'Obsession', 'Normal', 'Anxiety', 'Obsession', 'Normal', 'Obsession', 'Anxiety', 'Normal', 'Normal', 'Obsession', 'Obsession', 'Normal', 'Normal', 'Anxiety', 'Anxiety', 'Normal', 'Obsession', 'Normal', 'Anxiety', 'Normal', 'Normal', 'Anxiety', 'Normal', 'Obsession', 'Obsession', 'Anxiety', 'Anxiety', 'Normal')
X <- matrix(c(0.5, -0.4, 1, -0.8, -1.8, 1.7, -1, 0.2, 2.3, 1, -0.7, -0.3, 0.1, -0.8, 1.9, -0.5, -1.4, -1.6, -0.1, -0.8, -0.3, -2.2, 0.1, -1.5, -2.1, 0.5, 0.5, -0.2, -1.2, -1.6, 0.6, -1.1, -0.5, 0.2, 1.2, -0.9, 1.4, -0.4, -2.1, -2.7, -1.4, 0, -1.1, -0.7, -0.6, -1.1, -2, -1.4, 1.8, 0.7, -2.1, -2.5, -1, 0.5, -1.5, -0.4, 1.9, 0.5, -0.7, -0.7, 0.6, -0.5, -1, 1.8, 1.9, -1.3, -0.5, -2.2, 1.5, -0.9, -0.9, 0, -1.1, -0.3, -1.1, 0.7, -1.2, -2.6, 0, -0.5, -1, 0.2, -0.7, -2.6, 0.5, -0.3, -0.6, 0.3, 0.3, -0.7, -0.6, -0.9, -1, 0.4, 0.7, 0.4, -3.7, -1.7, 1.5, 2.6, 1.1, -0.2, 0.1, -1.2, -0.2, -2.4, -3.5, -2), 36, 3, byrow=TRUE, dimnames=list(NULL, c("A","B","C")))
neurotic <- cbind(data.frame(Diagnosis=G), X)
write.csv(neurotic, "neurotic.csv", row.names=FALSE)

library(MASS)
reframe <- data.frame(A=-0.5, B=-0.4, C=-0.4)
# Using LDA:
(neuroticlda <- lda(Diagnosis~., neurotic, prior=c(1,1,1)/3))
predict(neuroticlda, newdata=reframe)

################### b ii ###########################
table(neurotic$Diagnosis,predict(neuroticlda)$class)

ans_b_ii <- matrix(c(12,4,0,3,6,1,0,0,10),3,3,byrow=TRUE) # 3*3

################### b iii ########################
library(heplots)
library(dplyr)

neurotic.mlm <-lm(as.matrix(select(neurotic,-Diagnosis))~Diagnosis,
                  data=neurotic)
boxM(neurotic.mlm)



#################### c #####################
library(e1071)
set.seed(1)

summary(tuned.svm <- tune.svm(factor(Diagnosis)~.,data=neurotic,kernel="radial", gamma = 10^(-1:1), cost = 10^(-1:1)))
#Choose the one with the lowest error rate, that is, when ganma is equal to 0.1 and cost is equal to 10
#In addition, test other values to compare to see if this is the highest score
svm(factor(Diagnosis)~.,data=neurotic, kernel="radial", gamma=0.1, cost = 1, cross=5)$tot.accuracy
#61.11111
svm(factor(Diagnosis)~.,data=neurotic, kernel="radial", gamma=0.1, cost = 10, cross=5)$tot.accuracy
#63.88889
svm(factor(Diagnosis)~.,data=neurotic, kernel="radial", gamma=0.5, cost = 10, cross=5)$tot.accuracy
#52.77778
svm(factor(Diagnosis)~.,data=neurotic, kernel="radial", gamma=1, cost = 10, cross=5)$tot.accuracy
#55.55556
svm(factor(Diagnosis)~.,data=neurotic, kernel="radial", gamma=5, cost = 10, cross=5)$tot.accuracy
#52.77778
svm(factor(Diagnosis)~.,data=neurotic, kernel="radial", gamma=10, cost = 10, cross=5)$tot.accuracy
#41.66667
svm(factor(Diagnosis)~.,data=neurotic, kernel="radial", gamma=0.1, cost = 30, cross=5)$tot.accuracy
#58.33333
svm(factor(Diagnosis)~.,data=neurotic, kernel="radial", gamma=2, cost = 20, cross=5)$tot.accuracy
#63.88889
svm(factor(Diagnosis)~.,data=neurotic, kernel="radial", gamma=3, cost = 15, cross=5)$tot.accuracy
#50
svm(factor(Diagnosis)~.,data=neurotic, kernel="radial", gamma=2, cost = 5, cross=5)$tot.accuracy
#50
svm(factor(Diagnosis)~.,data=neurotic, kernel="radial", gamma=2, cost = 50, cross=5)$tot.accuracy
#61.11111
svm(factor(Diagnosis)~.,data=neurotic, kernel="radial", gamma=9, cost = 50, cross=5)$tot.accuracy
#38.88889
svm(factor(Diagnosis)~.,data=neurotic, kernel="radial", gamma=3, cost = 70, cross=5)$tot.accuracy
#47.22222
svm(factor(Diagnosis)~.,data=neurotic, kernel="radial", gamma=5, cost = 20, cross=5)$tot.accuracy
#50
#Choose the ganma=0.1, cost=10
summary(SVM<- svm(factor(Diagnosis)~.,data=neurotic, kernel="radial", gamma=0.1, cost = 10, cross=5))

reframe <- data.frame(A=-0.5, B=-0.4, C=-0.4)
(obpred <- predict(SVM, reframe))

