library(e1071) # svm()
library(pROC) # ROC curves
library(GGally)
library(dplyr)
set.seed(0)


G <- c('Obsession', 'Anxiety', 'Normal', 'Normal', 'Obsession', 'Anxiety', 'Obsession', 'Anxiety', 'Normal', 'Obsession', 'Normal', 'Anxiety', 'Obsession', 'Normal', 'Anxiety', 'Obsession', 'Obsession', 'Obsession', 'Obsession', 'Normal', 'Obsession', 'Anxiety', 'Anxiety', 'Obsession', 'Anxiety', 'Normal', 'Obsession', 'Normal', 'Normal', 'Anxiety', 'Obsession', 'Obsession', 'Obsession', 'Normal', 'Anxiety', 'Normal', 'Obsession', 'Normal')
X <- matrix(c(0, 0.1, 2.5, 1.6, -1.3, 4.2, 0, -0.5, -1.1, 1.7, 0.8, 0.1, -1.2, -0.6, 0.6, 0.2, -1.1, -1.1, -0.5, -3.1, -0.5, 1.9, -0.2, 3.1, 0.7, 0.4, -0.2, 1.5, -0.8, 2.4, 0.9, -1.1, 2.6, 1.2, -1, -0.5, -0.2, 0.1, 0.9, 1.2, 0.5, -1, 1.5, 0.2, 1.2, 0.3, 0.4, 3.2, -0.4, -0.4, 4.3, 0.5, -0.5, 0.8, 0.3, 0.6, 0.9, -0.1, 0.7, 0.2, -0.1, -0.9, 1.1, 1.1, 0.1, -4.5, 1.7, 0.5, 0.4, 0.6, 1.1, 2.2, 0.6, -1.4, 0.6, 1.7, 0.4, 0.2, 1.3, 0.1, -0.2, -0.4, -2.5, -2, 0.6, 0.6, -0.5, 1.6, -0.3, 3, 1.5, 0.4, 5.5, -0.1, -0.2, 0.9, 0.5, 0.2, 2.1, 1.4, 0.9, -1.8, 0.4, -0.8, -0.6, 0.3, -1.5, -0.2, -0.4, -1.2, 3.5, 0.7, 1.4, -0.6), 38, 3, byrow=TRUE, dimnames=list(NULL, c("A","B","C")))
neurotic <- cbind(data.frame(Diagnosis=G), X)%>% mutate(Diagnosis=factor(Diagnosis))
summary(svm.radial_8_10 <- svm(Diagnosis~.,data=neurotic,kernel="radial", gamma=8, ,cost=10,cross=10))# 47.36842 
summary(svm.radial<- svm(Diagnosis~.,data=neurotic,kernel="radial", gamma=9, cross=5))# 50 
summary(svm.radial<- svm(Diagnosis~.,data=neurotic,kernel="radial", gamma=15, cross=5))# 42 
summary(svm.radial<- svm(Diagnosis~.,data=neurotic,kernel="radial", gamma=15, cross=5))# 42 
summary(svm.radial<- svm(Diagnosis~.,data=neurotic,kernel="radial", gamma=5, cross=5))# 52.63158
summary(svm.radial<- svm(Diagnosis~.,data=neurotic,kernel="radial", gamma=5, cross=3))# 28.94737
summary(svm.radial<- svm(Diagnosis~.,data=neurotic,kernel="radial", gamma=5, cross=10))# 50
summary(svm.radial<- svm(Diagnosis~.,data=neurotic,kernel="radial", gamma=5, cross=7))# 52.63158
summary(svm.radial<- svm(Diagnosis~.,data=neurotic,kernel="radial", gamma=4.5, cross=7))# 52.63158
summary(svm.radial<- svm(Diagnosis~.,data=neurotic,kernel="radial", gamma=4.5, cross=6))# 55.26316
summary(svm.radial<- svm(Diagnosis~.,data=neurotic,kernel="radial", gamma=4.5, cross=5.5))# 50
summary(svm.radial<- svm(Diagnosis~.,data=neurotic,kernel="radial", gamma=4, cross=6))# 57.89474
summary(svm.radial<- svm(Diagnosis~.,data=neurotic,kernel="radial", gamma=2, cross=6))# 55.26316
summary(svm.radial<- svm(Diagnosis~.,data=neurotic,kernel="radial", gamma=3, cross=6))# 60.52632
summary(svm.radial<- svm(Diagnosis~.,data=neurotic,kernel="radial", gamma=3, cross=6))# 60.52632



summary(tuned.svm <- tune.svm(Diagnosis~.,data=neurotic,kernel="radial", gamma = 3^(-1:1), cost = 6^(-1:1)))
tuned.svm$best.model
############## 3 ##########################
nd <- data.frame(A=0, B=-0.3, C=-0.6)

predict(tuned.svm$best.model, newdata=nd, decision.values=TRUE)
