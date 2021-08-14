S_matrix <- matrix(c(6,4,4,9),nrow=2, byrow=T)
n = 13
Mean = c(-1.4,-0.3)
v = 0.1 # 1-0.9(=90%)
dimension = 2

#a
oval_matrix <- n * solve(S_matrix) *(n-dimension) / (dimension*(n-1)*qf(1-v,dimension,n-dimension))
eigen_matrix <- eigen(oval_matrix)
eigen_value <- eigen_matrix$values
eigen_vector <- eigen_matrix$vectors

a = function(Mean,eigen_value,eigen_vector){
  southeast_a = Mean - 1/sqrt(eigen_value[1]) * eigen_vector[,1]
  northwest_a = Mean + 1/sqrt(eigen_value[1]) * eigen_vector[,1]
  northeast_a = Mean - 1/sqrt(eigen_value[2]) * eigen_vector[,2]
  southwest_a = Mean + 1/sqrt(eigen_value[2]) * eigen_vector[,2]
  
  print(southeast_a)
  print(northwest_a)
  print(northeast_a)
  print(southwest_a)
}
a(Mean,eigen_value,eigen_vector)

#b
oval_matrix_b <- n * solve(S_matrix) / (qchisq(1-v,dimension))
eigen_matrix_b <- eigen(oval_matrix_b)
eigen_value_b <- eigen_matrix_b$values
eigen_vector_b <- eigen_matrix_b$vectors

a = function(Mean,eigen_value_b,eigen_vector_b){
  southeast_b = Mean - 1/sqrt(eigen_value_b[1]) * eigen_vector_b[,1]
  northwest_b = Mean + 1/sqrt(eigen_value_b[1]) * eigen_vector_b[,1]
  northeast_b = Mean - 1/sqrt(eigen_value_b[2]) * eigen_vector_b[,2]
  southwest_b = Mean + 1/sqrt(eigen_value_b[2]) * eigen_vector_b[,2]
  
  print(southeast_b)
  print(northwest_b)
  print(northeast_b)
  print(southwest_b)
}
b(Mean,eigen_value_b,eigen_vector_b)

#c
c = function(Mean,dimension,n,S_matrix){
  lower_u1_c = Mean[1] - sqrt((dimension*(n-1)*qf(1-v,dimension,n-dimension)) / (n*(n-dimension)) * S_matrix[1,1] )
  upper_u1_c = Mean[1] + sqrt((dimension*(n-1)*qf(1-v,dimension,n-dimension)) / (n*(n-dimension)) * S_matrix[1,1] )
  
  lower_u2_c = Mean[2] - sqrt((dimension*(n-1)*qf(1-v,dimension,n-dimension)) / (n*(n-dimension)) * S_matrix[2,2] )
  upper_u2_c = Mean[2] + sqrt((dimension*(n-1)*qf(1-v,dimension,n-dimension)) / (n*(n-dimension)) * S_matrix[2,2] )
  
  print(lower_u1_c)
  print(upper_u1_c)
  
  print(lower_u2_c)
  print(upper_u2_c)
}

c(Mean,dimension,n,S_matrix)

#d
m = 2

d = function(Mean,m,n,S_matrix){
  lower_u1_d = Mean[1] - qt(1-v/(2*m),n-1) * (S_matrix[1,1]/n)
  upper_u1_d = Mean[1] + qt(1-v/(2*m),n-1) * (S_matrix[1,1]/n)
  
  lower_u2_d = Mean[2] + qt(1-v/(2*m),n-1) * (S_matrix[2,2]/n)
  upper_u2_d = Mean[2] - qt(1-v/(2*m),n-1) * (S_matrix[2,2]/n) 
    
  print(lower_u1_d)
  print(upper_u1_d)
  
  print(lower_u2_d)
  print(upper_u2_d)
  
}

d(Mean,m,n,S_matrix)