#
library(mvtnorm)

### 1. Poisson regression - the MCMC way.
dat = read.table("C:/Users/Sam/Desktop/732A91 Bayesian Learning/Module 4 - Model Inference and Variable Selection/eBayNumberOfBidderData.dat",
                  header = T)

### 1(a) Obtain the maximum likelihood estimator of beta 
### in the Poisson regression model

# [Hint: glm() adds its own intercept
# so don’t input the covariate Const].
# added a zero in the model so that R doesn’t add an extra intercept
# (we already have an intercept term from the Constant feature).

fit <- glm(nBids ~ 0 + ., data=dat, family=poisson)
fit

### 1(b) Bayesian analysis of the Poisson regression
x = as.matrix(dat[,-1])
y = as.vector(dat$nBids)

covNames <- names(dat)[2:length(names(dat))]
nPara <- ncol(x)

# prior
mu <- as.vector(rep(0,nPara)) # Prior mean vector
Sigma <- 100*solve(t(x)%*%x)

# Log Posterior
LogPostPoisson <- function(betaVect,y,X,mu,Sigma){
  
  nPara <- length(betaVect)
  linPred <- X%*%betaVect
  
  # log-likelihood of Possion Regression                                    
  logLik <- sum(y*linPred - exp(linPred)) 
  #drop(no beta): -log(factorial(y))
  
  # prior
  # library(mvtnorm)
  logPrior <- dmvnorm(betaVect, mu, Sigma, log=TRUE)
  
  # add the log prior and log-likelihood together to get log posterior
  return(logLik + logPrior)
}

initVal <- as.vector(rep(0,ncol(x))) #as it is vector,no need t(x)%*%initVal
OptimResults<-optim(initVal,LogPostPoisson,gr=NULL,
                    y,x,mu,Sigma,
                    method=c("BFGS"),control=list(fnscale=-1),hessian=TRUE)

# Printing the results to the screen
postMode <- OptimResults$par
postCov <- -solve(OptimResults$hessian) # Posterior covariance matrix is -inv(Hessian)
names(postMode) <- covNames # Naming the coefficient by covariates
approxPostStd <- sqrt(diag(postCov)) # Computing approximate standard deviations.
names(approxPostStd) <- covNames # Naming the coefficient by covariates

# posterior density is approximately multivariate normal.
par(mfrow = c(3,3))
for(i in 1:length(as.vector(rep(0,ncol(x))))){
  # normal approximation
  sd_i <- approxPostStd[i]
  betaSeq <- seq(postMode[i] - 4*sd_i,
                 postMode[i] + 4*sd_i,
                 length = 1000)
  plot(betaSeq,dnorm(x = betaSeq,mean = postMode[i],sd = sd_i),
       type = "l", main = names(postMode)[i],col="blue",
       ylab = "",xlab = expression(beta))

}
par(mfrow = c(1,1))


### 1(c) simulate actual posterior of beta 
### by Metropolis algorithm

metropolis <- function(logPostFunction, constant, data, initVal, nDraws, ...) {
  thetaDraws <- matrix(nrow = (nDraws+1),
                       ncol = length(initVal))
  thetaDraws[1,] <- initVal
  # Metropolis-Hastings
  for (i in 2:(nDraws+1)) {
    # Proposal distribution
    theta_p <- rmvnorm(n=1, mean=thetaDraws[(i-1),], 
                       sigma=constant*(-solve(OptimResults$hessian)))
    
    # Compute alpha
    alpha <- min(1, logPostFunc(theta=list(proposal=theta_p,current=thetaDraws[(i-1),]),c=constant))
    
    # Accept with probability
    u <- runif(n = 1)
    if (u < alpha) thetaDraws[i,] <- theta_p
    else thetaDraws[i,] <- thetaDraws[(i-1),]
  }
  return(thetaDraws)
}

# (log) posterior function
logPostFunc <- function(theta, c, X=x, y=as.vector(dat$nBids), ...){

  theta_p <- theta$proposal
  theta_c <- theta$current
  nPara <- length(theta_p)
  
  linComb_p <- X%*%as.vector(theta_p)
  linComb_c <- X%*%as.vector(theta_c)
  
  # proposal
  logPrior_p <- dmvnorm(theta_p, mu, Sigma, log=TRUE)
  logPost_p <- sum(y*linComb_p - exp(linComb_p)) + logPrior_p
  
  # current
  logPrior_c <- dmvnorm(theta_c, mu, Sigma, log=TRUE) #+ log(c)
  logPost_c <- sum(y*linComb_c - exp(linComb_c)) + logPrior_c
  
  # ratio of posterior densities
  return(exp(logPost_p - logPost_c))
}


MH <- metropolis(logPostFunction = logPostFunc,
                 initVal = as.vector(rep(0,ncol(x))),
                 c = 1.3,
                 nDraws = 5000)

# Assess MCMC convergence by exp(beta)
par(mfrow = c(3,3))
for(i in 1:ncol(MH)){
  
  # plot metroplis simulations 1(c)
  plot(exp(MH[,i]),xlab="Iteration", ylab = "expression(beta)",
       type="l",main = names(postMode)[i])
  
}
par(mfrow = c(1,1))

# Compute the posterior distribution of exp(beta)
par(mfrow = c(3,3))
for(i in 1:ncol(MH)){

  # plot metroplis simulations 1(c)
  hist(exp(MH[,i]),ylab = "",xlab = expression(beta),
       main = names(postMode)[i],breaks = 50,freq = FALSE)
  # compare with the approximate results in b).
  # plot approximately multivariate normal 1(b)
  #sd_i <- approxPostStd[i]
  #betaSeq <- seq(postMode[i] - 4*sd_i,
   #              postMode[i] + 4*sd_i,
  #               length = 1000)
  #lines(betaSeq,dnorm(x = betaSeq,mean = postMode[i],sd = sd_i),
   #     type = "l",lwd = 2,col = "red")
}
par(mfrow = c(1,1))


################################################################
# This is the log posterior density of the beta(s+a,f+b) density
# Bernoulli model with a Beta prior
LogPostBernBeta <- function(theta, s, f, a, b){
  logPost <- (s+a-1)*log(theta) + (f+b-1)*log(1-theta)
  return(logPost)
}
# Testing if the log posterior function works
s <- 8;f <- 2;a <- 1;b <- 1
logPost <- LogPostBernBeta(theta = 0.1, s, f, a, b)
print(logPost)
# This is a rather useless function that takes the function myFunction,
# evaluates it at x = 0.3, and then returns two times the function value.
MultiplyByTwo <- function(myFunction, ...){
  x <- 0.3
  y <- myFunction(x,...)
  return(2*y)
}
# try if the MultiplyByTwo function works:
MultiplyByTwo(LogPostBernBeta,s,f,a,b)
#################################################################

### 1(d) simulate from the predictive distribution
### Use the MCMC draws from c)
x_pred = matrix(c(1,1,1,0,0,0,1,0.5),nrow = 1) 
# add constant
x_pred = cbind(1,x_pred)

beta_est <- as.vector(colMeans(MH))

y_pred <- rpois(n = 1000, lambda = exp(x_pred%*%beta_est))
hist(y_pred,breaks = 50,xlab = "nBids",freq = F)

probNoBidders <- (sum(y_pred == 0))/(length(y_pred))
cat("The probability that there are no bidders in this new auction is", probNoBidders)


