#er

# 矩阵的幂
M <- matrix(c(5,3,3,8), nrow=2)

Eigen <- eigen(M)

# SVD：M = V D V^T

V = Eigen$vectors

D = diag(Eigen$values)


M - V %*% D %*% t(V) #存在极小误差可以视作全0

# M^n = V D^n V^T


# M^3


D3 <- diag(c(956.8661, 31.13393)) # or diag(Eigen$values^3)

M %*% M %*% M - V %*% D3 %*% t(V) # 误差大了些但相对依旧可以视为全0


#M^1/3
#M^-1/3

(V %*% diag(Eigen$values^(1/3)) %*% t(V)) %*% (V %*% diag(Eigen$values^(-1/3)) %*% t(V))


























