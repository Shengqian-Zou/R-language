coordinates = function(sigma,n,centre_x,X2_square_n){
  lambda1 = eigen(sigma)$values[1]
  lambda2 = eigen(sigma)$values[2]
  e1 = eigen(sigma)$vectors[,1]
  e2 = eigen(sigma)$vectors[,2]
  
  axes1 = sqrt(lambda1 * X2_square_n / n) * e1
  axes2 = sqrt(lambda2 * X2_square_n / n) * e2
  
  southeast = centre_x - axes2
  northwest = centre_x + axes2
  northeast = centre_x + axes1
  southwest = centre_x - axes1
  
  
  print(southeast)
  print(northwest)
  print(northeast)
  print(southwest)
  
}

sigma = matrix(c(8,4,4,8),nrow = 2)
#A random sample of 19 observations
n = 19
# a sample mean [-3, -1]
centre_x = matrix(c(-3,-1),nrow = 2)

#The F distribution table of level 90%
X2_F_n = 0
#The chi-square distribution table of level 90%
X2_chi_square_n = 4.61


coordinates(sigma,n,centre_x,X2_chi_square_n)


