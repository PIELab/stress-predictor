#!/usr/bin/Rscript

#EM algorithm is used to optimize the initial parameters.
#Parameter for a Poisson - HMM 
#gamma = The transition matrix,
#lambda= The Poisson means
#delta = The initial stationary distribution


#Forward and Backward algorithms

pois.HMM.lalphabeta<-function(x,m,lambda,gamma,delta=NULL)  
{                                                           
 if(is.null(delta))delta<-solve(t(diag(m)-gamma+1),rep(1,m))   
 n          <- length(x)                                    
 lalpha     <- lbeta<-matrix(NA,m,n)                       
 allprobs   <- outer(x,lambda,dpois)                        
 foo        <- delta*allprobs[1,]                           
 sumfoo     <- sum(foo)                                    
 lscale     <- log(sumfoo)                                 
 foo        <- foo/sumfoo                                   
 lalpha[,1] <- log(foo)+lscale                              
 for (i in 2:n)                                             
   {                                                        
   foo        <- foo%*%gamma*allprobs[i,]                   
   sumfoo     <- sum(foo)                                   
   lscale     <- lscale+log(sumfoo)                         
   foo        <- foo/sumfoo                                 
   lalpha[,i] <- log(foo)+lscale                            
   }                                                        
 lbeta[,n]  <- rep(0,m)                                     
 foo        <- rep(1/m,m)                                   
 lscale     <- log(m)                                       
 for (i in (n-1):1)                                         
   {                                                        
   foo        <- gamma%*%(allprobs[i+1,]*foo)               
   lbeta[,i]  <- log(foo)+lscale                            
   sumfoo     <- sum(foo)                                   
   foo        <- foo/sumfoo                                 
   lscale     <- lscale+log(sumfoo)                         
   }                                                        
 list(la=lalpha,lb=lbeta)                                   
}

#COMPUTIING THE PARAMETERS OF THE DISTRIBUTION USING EM ALGORITHM
#Provide initial values for lambda, gama, and delta, the algoritm
#optimize these values based on the obsevation x

pois.HMM.EM <- function(x,m,lambda,gamma,delta, maxiter=1000,tol=1e-6)         
{                                                          
 n              <- length(x)                                        
 lambda.next    <- lambda                                   
 gamma.next     <- gamma                                    
 delta.next     <- delta                                   
 for (iter in 1:maxiter)                                    
   {                                                        
   lallprobs    <- outer(x,lambda,dpois,log=TRUE)           
   fb  <-  pois.HMM.lalphabeta(x,m,lambda,gamma,delta=delta)   
   la  <-  fb$la                                            
   lb  <-  fb$lb                                            
   c   <-  max(la[,n])                                      
   llk <- c+log(sum(exp(la[,n]-c)))                         
   for (j in 1:m)                                           
   {                                                       
     for (k in 1:m)                                         
     {                                                      
       gamma.next[j,k] <- gamma[j,k]*sum(exp(la[j,1:(n-1)]+   
                          lallprobs[2:n,k]+lb[k,2:n]-llk))  
     }                                                      
   lambda.next[j] <- sum(exp(la[j,]+lb[j,]-llk)*x)/         
                     sum(exp(la[j,]+lb[j,]-llk))            
   }                                                       
   gamma.next <- gamma.next/apply(gamma.next,1,sum)         
   delta.next <- exp(la[,1]+lb[,1]-llk)                     
   delta.next <- delta.next/sum(delta.next)                 
   crit       <- sum(abs(lambda-lambda.next)) +             
                 sum(abs(gamma-gamma.next)) +               
                 sum(abs(delta-delta.next))                 
   if(crit<tol)                                             
     {                                                      
     np     <- m*m+m-1                                      
     AIC    <- -2*(llk-np)                                  
     BIC    <- -2*llk+np*log(n)                             
     return(list(lambda=lambda,gamma=gamma,delta=delta,    
           mllk=-llk,AIC=AIC,BIC=BIC))                     
     }                                                      
   lambda     <- lambda.next                               
   gamma      <- gamma.next                                 
   delta      <- delta.next                                
   }                                                        
 print(paste("No convergence after",maxiter,"iterations"))  
 NA                                                         
}                             

pois.HMM.state_prediction <- function(x,m,lambda,gamma,delta,H=1)              
{                                                           
 if(is.null(delta))delta<-solve(t(diag(m)-gamma+1),rep(1,m))  
 n          <- length(x)                                    
 fb         <- pois.HMM.lalphabeta(x,m,                     
                 lambda,gamma,delta=delta)                  
 la         <- fb$la                                       
 c          <- max(la[,n])                                 
 llk        <- c+log(sum(exp(la[,n]-c)))                    
 statepreds <- matrix(NA,ncol=H,nrow=m)                     
 foo1       <- exp(la[,n]-llk)                              
 foo2       <- diag(m)                                      
 for (i in 1:H)                                             
   {                                                        
   foo2           <- foo2%*%gamma                           
   statepreds[,i] <- foo1%*%foo2                            
   }                                                        
 statepreds                                                 
}         
 
