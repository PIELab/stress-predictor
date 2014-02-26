source("predict.r")
#Example 1, 242 samples, 1 observation per state, 3 states.
x <- read.table("soap.txt")
x <- data.matrix(x)
x <- as.vector(x)


m = 3
lambda = c(3,5,7)
gamma = rbind(c(0.9,0.05,0.05),c(0.05,0.9,0.05),c(0.05,0.05,0.9))
delta<-solve(t(diag(m)-gamma+1),rep(1,m))
s<-pois.HMM.EM(x,m,lambda,gamma,delta, maxiter=1000,tol=1e-6)
res3 <-pois.HMM.state_prediction(x,m,s$lambda ,s$gamma ,s$delta ,H=1)

#EXAMPLE #2
#Data from ECG  200 samples, 1 observation per state, 2 states, data discretized
#multiplying by 100 and the applying the ceilling function

y <- read.table("data4.txt")
y <- data.matrix(y)
y <- as.vector(y)

y<-ceiling(100*y)
y<-y[1:500]

#s<-pois.HMM.forecast(x,m,lambda,gamma, delta=NULL,xrange=NULL,H=1)
m=2
lambda = c(50,80)
gamma = rbind(c(0.7,0.3),c(0.3, 0.7))
#delta<-solve(t(diag(m)-gamma+1),rep(1,m))
delta=c(0.5, 0.5)
s<-pois.HMM.EM(y,m,lambda,gamma,delta, maxiter=1000,tol=1e-6)
res3 <-pois.HMM.state_prediction(y,m,s$lambda ,s$gamma ,s$delta ,H=1)
