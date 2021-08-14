Sigma <- matrix(c(209, -55, -12, 0, -55, 363, -10, 13, -12, -10, 52, -52, 0, 13, -52, 169), 4, 4)
n<-30
p<-4
Sigma_h0<-diag(diag(Sigma))
V<-n*Sigma
temp<- exp(-1/2*sum(diag(solve(Sigma_h0)%*%V)))
L0<-((2*pi)^(-n*p/2))*det(Sigma_h0)^(-n/2)*temp
L1<-((2*pi)^(-n*p/2))*(n^(n*p/2))*(det(V)^(-n/2))*exp(-1/2*n*p)


Lambda<-L0/L1
Statistics = -2 * log(L0/L1)#h0 chisq(df)~test statistics
df <- p*(p+1)/2-p

# critical value, if test statistic value >cr value reject h0
qchisq(0.95,df)#>-2log(L0/L1),h0 yes, or h1 yes
(ans_e_chisq <- Statistics)
(ans_e_df <-df)
(ans_e_pval <- pchisq(Statistics,df, lower.tail = FALSE)) #because <5%, so h1 right






