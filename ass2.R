Sigma <- matrix(c(209, -55, -12, 0, -55, 363, -10, 13, -12, -10, 52, -52, 0, 13, -52, 169), 4, 4)
Sigma_12_34 <- Sigma[1:2,1:2]-Sigma[1:2,3:4] %*% solve(Sigma[3:4,3:4]) %*% Sigma[3:4,1:2]
V <- diag(1/sqrt(diag(Sigma_12_34)))
Rho_12_34 <- V %*% Sigma_12_34 %*% V
Rho_12_34

library(ggm)
ans_a <- pcor(c(1:2,3:4), Sigma)

# b
Rignt_down <- Sigma[2:4,2:4]
Sigma_0 <- Sigma[2:4,1]
Sigma_0
Sigma_Y <- Sigma[1,1]
Sigma_Y
t(Sigma_0)

R <- t(Sigma_0) %*% solve(Rignt_down) %*% Sigma_0 / Sigma_Y
R
PCCs <- diag(1/sqrt(diag(Sigma))) %*% Sigma %*% diag(1/sqrt(diag(Sigma)))
1-1/solve(RHO)[1,1]

# c

matpow <- function(A, p){
  with(eigen(A), vectors %*% diag(values^p, nrow(A)) %*% t(vectors))
}

(ccdecomp <- eigen(matpow(Sigma[3:4,3:4],-1/2) %*% Sigma[3:4,1:2] %*% solve(Sigma[1:2,1:2]) %*% Sigma[1:2,3:4] %*% matpow(Sigma[3:4,3:4],-1/2)))
(ccors <- sqrt(ccdecomp$values))# 方法1

(ccdecomp2 <- eigen(matpow(RHO[3:4,3:4],-1/2) %*% RHO[3:4,1:2] %*% solve(RHO[1:2,1:2]) %*% RHO[1:2,3:4] %*% matpow(RHO[3:4,3:4],-1/2)))
(ccors <- sqrt(ccdecomp2$values))# 方法2

# c2
library(CCP)
n1 <- 2
n2 <- 2
s <- 30
a <- p.asym(ccors, s, n1, n2, tstat = "Wilks")

# 4
pca <- prcomp(Sigma, scale = TRUE)
s.pca <- summary(pca)
ans_d_iv <- s.pca$importance[2, ]

# zwk
PCA_E1<-eigen(Sigma)
#(d_1)
pcvars<-PCA_E1$values
PCA_E1$vectors[,1]
PCA_E1$vectors[,2]

#(d_2)
# c=90% of variance explained
(var.explained <- cumsum(pcvars)/sum(pcvars))
min(which(var.explained>=0.9))
 #dii
s.eigen <- eigen(Sigma)
s <- c()
for (i in 1:4){
  s = append(s, sum(s.eigen$values[1:i]) / sum(s.eigen$values))
}
df <- data.frame(
  PC = c('PC1', 'PC2', 'PC3', 'PC4'),
  Cumulative = s )
knitr::kable(df, caption = 'Importance of componets')
plot(seq_along(s), s.eigen$values / sum(s.eigen$values), type="o", ylim=c(0,1), xlab="k", ylab="Proportion of variance explained")
points(seq_along(s), s, lty=2, type="o")
legend("right", lty=c(1,2), legend=c("Individual","Cumulative"))
abline(h=c(1/4, 0.9), lty=3)
#(d_3)
# Kaiser's rule:
max(which(pcvars>=1))

#d4
(Sigma.pc <- prcomp(Sigma, scale=TRUE))
(pcvars <- Sigma.pc$sdev^2) # Eigenvalues
Vinvsqrt<-diag(1/sqrt(diag(Sigma)))
RHO<-Vinvsqrt%*%Sigma%*%Vinvsqrt
E2 <- eigen(RHO)
pcvars <- E2$values
(var.explained <- pcvars/sum(pcvars))

