One_half_power <- function(sigma){
  eigenvalues = eigen(sigma)$values
  P <- eigen(sigma)$vectors
  P_1 <- solve(P)
  sqrt_lambda <- sqrt(diag(eigenvalues))
  result <- P %*% sqrt_lambda %*% P_1
  result
  return (result)
}

sigma <- matrix(c(8,1,1,8),nrow = 2)
sigma_1 <- solve(sigma)
eigenvalues = eigen(sigma)$values
P <- eigen(sigma)$vectors



print("Its characteristic polynomial (in terms of lambda) is: ")
print("lambda^2 -16lambda +63")

print("The eigenvalues of this matrix (in decreasing order) are")
eigenvalues
print("The corresponding normalised eigenvectors of this matrix are:")
P

print("Calculate ¦²1/2:")
One_half_power(sigma)

print("Calculate ¦²???1/2:")
One_half_power(sigma_1)

