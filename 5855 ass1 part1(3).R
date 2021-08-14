#san


S <- matrix(c(5,3,3,9),nrow=2, byrow=T)

n = 10

p = 2 # dimension 废话当然�?2维度

X_bar = c(3,2)

alpha = 0.1

#a
#S^-1 最快是用solve()

#椭圆的定义矩�?
M <- n * solve(S) *(n-p) / (p*(n-1)*qf(1-alpha,p,n-p))


#需要特征矢�?
EIGEN <- eigen(M)

eigenval <- EIGEN$values
eigenvec <- EIGEN$vectors


#i=1
X_bar + 1/sqrt(eigenval[1]) * eigenvec[,1]
#(x,y)=(1.640491 2.727587)
X_bar - 1/sqrt(eigenval[1]) * eigenvec[,1]
#(x,y)=(4.359509 1.272413)
#i=2
X_bar + 1/sqrt(eigenval[2]) * eigenvec[,2]
#(x,y)=(1.7139232 -0.4030565)
X_bar - 1/sqrt(eigenval[2]) * eigenvec[,2]
#(x,y)=(4.286077 4.403056)

#请自行对号入�? 坐标对应椭圆端点


#b
#同样套路


Sigma <- S #一个东�?

#椭圆的定义矩�?
M <- n * solve(Sigma) / (qchisq(1-alpha,p))

EIGEN <- eigen(M)

eigenval <- EIGEN$values
eigenvec <- EIGEN$vectors



#i=1
X_bar + 1/sqrt(eigenval[1]) * eigenvec[,1]
#(x,y)=(1.897659 2.589955)
X_bar - 1/sqrt(eigenval[1]) * eigenvec[,1]
#(x,y)=(4.102341 1.410045)
#i=2
X_bar + 1/sqrt(eigenval[2]) * eigenvec[,2]
#(x,y)=(1.95720073 0.05151173)
X_bar - 1/sqrt(eigenval[2]) * eigenvec[,2]
#(x,y)=(4.042799 3.948488)


#c

l=c(1,0) #x

t(l) %*%X_bar + sqrt((p*(n-1)*qf(1-alpha,p,n-p)) / (n*(n-p)) * t(l)%*%S%*%l )
t(l) %*%X_bar - sqrt((p*(n-1)*qf(1-alpha,p,n-p)) / (n*(n-p)) * t(l)%*%S%*%l )
#(1.128568,4.871432)


l=c(0,1) #y

t(l) %*%X_bar + sqrt((p*(n-1)*qf(1-alpha,p,n-p)) / (n*(n-p)) * t(l)%*%S%*%l )
t(l) %*%X_bar - sqrt((p*(n-1)*qf(1-alpha,p,n-p)) / (n*(n-p)) * t(l)%*%S%*%l )
#如下一�?
X_bar[2] + sqrt((p*(n-1)*qf(1-alpha,p,n-p)) / (n*(n-p)) * S[2,2] )
X_bar[2] - sqrt((p*(n-1)*qf(1-alpha,p,n-p)) / (n*(n-p)) * S[2,2] )

#(-0.5107894,4.510789)


#d
m = 2


#X
X_bar[1] + qt(1-alpha/(2*m),n-1) * (S[1,1]/n)
X_bar[1] - qt(1-alpha/(2*m),n-1) * (S[1,1]/n)
#(1.868921,4.131079) 比之前的(1.128568,4.871432)�?

#y
X_bar[2] + qt(1-alpha/(2*m),n-1) * (S[2,2]/n)
X_bar[2] - qt(1-alpha/(2*m),n-1) * (S[2,2]/n)
#(-0.03594145,4.035941)
