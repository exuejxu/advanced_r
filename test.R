#
library(mvtnorm)
library(msm)

### 1.Normal model, mixture of normal model with semi-conjugate prior.
rain = read.table("C:/Users/Sam/Desktop/732A91 Bayesian Learning/Module 3 - MCMC and Variational Bayes/rainfall.dat",
                 header = F)

### (1a) Normal model.
x = rain[,1]
n = nrow(rain)

# set up the sup-initial parameters
# prior: mu~N(mu0,tao02)
mu0 <- 40
tao02 <- 1e5

# prior: sigma2~Inv-chi^2(nu0,sigma02)
nu0 <- 3
sigma02 <- 1000

# initial values
mu <- 40
sigma2 <- 2000
draws[1,1] <- mu     #best guess:mean(x)
draws[1,2] <- sigma2 #best guess:var(x)

nDraws <- 1000
draws <- matrix(NA,nDraws,2) 

for(i in 2:nDraws){
  
  # mu~N(mu_n,tao_n2)  
  tao_n2 = 1 / (n/sigma2+1/tao02)
  w = n/sigma2 / (n/sigma2+1/tao02)
  mu_n = w*mean(x) + (1-w)*mu0

  mu = rnorm(1,mean=mu_n,sd=sqrt(tao_n2))
  draws[i,1] <- mu

  # sigma2~N(nu_n,sigma_n2)
  nu_n = nu0 + n
  sigma_n2 = (nu0*sigma02 + sum((x-mu)^2)) / (nu_n)

  temp <- rchisq(n=1, df=nu_n) #chi-square
  sigma2 <- (nu_n*sigma_n2) / temp #scaled inversed chi-square
  draws[i,2] <- sigma2
}

plot(draws[,1],type="l",xlab = "Iterations",ylab = expression(mu))
plot(draws[,2],type="l",xlab = "Iterations",ylab = expression(sigma^2))

mean(draws[,1])
mean(draws[,2])
 
########## (1b) two-component Mixture normal model.
# Model options
nComp <- 2    # Number of mixture components

# Prior options
alpha <- 10*rep(1,nComp) # Dirichlet(alpha)
# prior of theta/ mu
muPrior <- rep(0,nComp) # Prior mean of theta
tau2Prior <- rep(10,nComp) # Prior std theta
# prior of sigma2
sigma2_0 <- rep(var(x),nComp) # s20 (best guess of sigma2)
nu0 <- rep(4,nComp) # degrees of freedom for prior on sigma2

# MCMC options
nIter <- 1000 # Number of Gibbs sampling draws
################   END USER INPUT ###############

###### Defining a function that simulates from the 
rScaledInvChi2 <- function(n, df, scale){
  return((df*scale)/rchisq(n,df=df))
}

####### Defining a function that simulates from a Dirichlet distribution
rDirichlet <- function(param){
  nCat <- length(param)
  thetaDraws <- matrix(NA,nCat,1)
  for (j in 1:nCat){
    thetaDraws[j] <- rgamma(1,param[j],1)
  }
  thetaDraws = thetaDraws/sum(thetaDraws) # Diving every column of ThetaDraws by the sum of the elements in that column.
  return(thetaDraws)
}

# Simple function that converts between two different representations of the group allocation
S2alloc <- function(S){
  n <- dim(S)[1]
  alloc <- rep(0,n)
  for (i in 1:n){
    alloc[i] <- which(S[i,] == 1)
  }
  return(alloc) #length(x), 1,2,2,1,...
}

# Initial value for the MCMC
nObs <- length(x)
S <- t(rmultinom(nObs, size = 1 , prob = rep(1/nComp,nComp))) # nObs-by-nComp matrix with component allocations.
probObsInComp <- rep(NA, nComp)
#mu
theta <- quantile(x, probs = seq(0,1,length = nComp)) 
#sigma2 
sigma2 <- rep(var(x),nComp) 

res_theta <- matrix(NA,nIter,nComp) 
res_sigma2 <- matrix(NA,nIter,nComp) 
res_theta[1,] <- theta
res_sigma2[1,] <- sigma2 

res_w <- matrix(1,nIter,nComp) 

for (k in 2:nIter){
  #message(paste('Iteration number:',k))
  alloc <- S2alloc(S) #function of convert to group allocation
  nAlloc <- colSums(S)
  # Update components probabilities
  w <- rDirichlet(alpha + nAlloc)
  res_w[k,] <- w 
  
  # Update theta's/ mu's
  for (j in 1:nComp){
    tau2Post <- 1 / (nAlloc[j]/sigma2[j] + 1/tau2Prior[j])
    wPrior <- (nAlloc[j]/sigma2[j]) / (nAlloc[j]/sigma2[j] + 1/tau2Prior[j])
    muPost <- wPrior*mean(x[alloc == j]) + (1-wPrior)*muPrior
    theta[j] <- rnorm(1, mean = muPost, sd = sqrt(tau2Post))
  }
  res_theta[k,] <- theta
  
  # Update sigma2's
  for (j in 1:nComp){
    df = nu0[j] + nAlloc[j]
    scale = (nu0[j]*sigma2_0[j] + sum((x[alloc==j]-theta[j])^2)) / df
    sigma2[j] <- rScaledInvChi2(1,df,scale)
  }
  res_sigma2[k,] <- sigma2
  
  # Update allocation
  for (i in 1:nObs){
    for (j in 1:nComp){
      probObsInComp[j] <- w[j]*dnorm(x[i], mean = theta[j], sd = sqrt(sigma2[j]))
    }
    S[i,] <- t(rmultinom(1, size = 1 , prob = probObsInComp/sum(probObsInComp)))
  }
}

# mu1: MCMC converge needs 2 plots 
par(mfrow = c(1,2))
plot(res_theta[,1], type = "l",xlab = "Iterations",ylab = expression(mu),
     main = expression(mu[1]))

t1 = res_theta[,1] 
res1 = c()
for(i in 1:length(t1)){
  res1[i] = mean(t1[1:i])
}
plot(res1, type = "l",xlab = "Iterations",ylab = expression(E(mu[i])))

# mu2
plot(res_theta[,2], type = "l",xlab = "Iterations",ylab = expression(mu),
     main = expression(mu[2]))
t2 = res_theta[,2] 
res2 = c()
for(i in 1:length(t2)){
  res2[i] = mean(t2[1:i])
}
plot(res2, type = "l",xlab = "Iterations",ylab = expression(E(mu[i])))

# sigma2_1
par(mfrow = c(1,2))
plot(res_sigma2[,1], type = "l",xlab = "Iterations",ylab = expression(sigma^2),
     main = expression(sigma[1]^2 ))

t3 = res_sigma2[,1] 
res3 = c()
for(i in 1:length(t3)){
  res3[i] = mean(t3[1:i])
}
plot(res3, type = "l",xlab = "Iterations",ylab = expression(E(sigma[i]^2)))

# sigma2_2
plot(res_sigma2[,2], type = "l",xlab = "Iterations",ylab = expression(sigma^2),
     main = expression(sigma[2]^2 ))

t4 = res_sigma2[,2] 
res4 = c()
for(i in 1:length(t4)){
  res4[i] = mean(t4[1:i])
}
plot(res4, type = "l",xlab = "Iterations",ylab = expression(E(sigma[i]^2)))

par(mfrow = c(1,1))

# w
plot(res_w[-1,1], type = "l",xlab = "Iterations",ylab = expression(w),
     main = expression(w))


### (1c) Graphical comparison.
# plot a histogram or kernel density estimate of the data.
hist(x, breaks = 50, main = "", freq = FALSE, xlab = "Perceptitation")

# plot Normal density in (a)
xSeq <- seq(min(x)-2*sd(x),max(x)+2*sd(x),length = 1000)
mu_a <- mean(draws[300:100,1])
sd_a <- sqrt(mean(draws[300:100,2]))
lines(xSeq, dnorm(xSeq, mean = mu_a, sd = sd_a), type = "l", col = "red")

# plot Mixture of normals density in (b).
w <- mean(res_w[300:1000,1])
mu_b <- colMeans(res_theta[300:1000,])        #[1]69.78689 2033.14458
sd_b <- sqrt(colMeans(res_sigma2[300:1000,])) #[1]8.353855 45.090405

mixDens <- w*dnorm(xSeq,mean=mu_b[1],sd=sd_b[1]) + 
  (1-w)*dnorm(xSeq,mean=mu_b[2],sd=sd_b[2])

lines(x = xSeq, mixDens, type = "l", lty = 1, col = "green")

legend("topright", bty = "n",
       legend = c("Data histogram","Normal density","Mixture density"), 
       col=c("black","red","green"), lwd=1, cex = 0.8)



########################
### 2. Probit regression
dat = read.table("C:/Users/Sam/Desktop/732A91 Bayesian Learning/Module 2 - Bayesian Regression and Classification/WomenWork.dat",
                 header = T)

y = as.vector(dat$Work)
x = as.matrix(dat[,-1])#already have!! 1st col: constant 
#### if not: x = cbind(1, x)
n = nrow(dat)

### 2(a) Gibbs sampler for the probit regression
Gibbs_pr <- function(muB,sigmaB,nIter=1000){
  
  # initial value
  u <- as.vector(rep(0,nrow(dat)))
  
  res_beta <- matrix(NA,nIter,ncol(x))
  res_u <- matrix(NA,nIter,nrow(dat))
  
  for(i in 1:nIter){
    
    # update beta
    sigmaB_hat <- solve(t(x)%*%x + solve(sigmaB))
    muB_hat <- sigmaB_hat %*% (t(x)%*%u + solve(sigmaB)%*%muB) ### u
 
    #library(mvtnorm)
    beta <- rmvnorm(n=1,mean=muB_hat,sigma=sigmaB_hat)
    res_beta[i,] <- beta
    
    # update u
    #library(msm)
    u <- rtnorm(nrow(x), mean=x%*%t(beta), lower=ifelse(y>0,0,-Inf), 
                upper=ifelse(y>0,Inf,0))
  }
  colnames(res_beta) <- colnames(x)
  return(list(u=res_u,beta=res_beta))
}


### 2(b) compute posterior of beta 
# using prior N(0,(tau^2)*I), tao=10
tau <- 10
covNames <- names(dat)[2:length(names(dat))]
nPara <- ncol(x) #8

# input prior
muB <- as.vector(rep(0,nPara)) #mean vector
sigmaB <- tau^2*diag(nPara)

res <- Gibbs_pr(muB,sigmaB) #call function

betaMeans <- colMeans(res$beta)
betaSd <- apply(res$beta, 2, sd)

par(mfrow = c(3,3))
for(i in 1:8){
  CI <- quantile(res$beta[,i], probs = c(0.025, 0.975))
  hist(res$beta[,i],breaks = 50,main = colnames(res$beta)[i],
       xlab = expression(beta),freq = FALSE)
  abline(v = betaMeans[i], col = "red")
  abline(v = CI, col = "blue", lty = 2)
  legend("topleft", legend = c(expression(paste("E[",Beta,"]")),"95% Equal tail CI"),
         col = c("red","blue"), lty = 1, bty = "n",cex = 0.6)
}
par(mfrow = c(1,1))

### 2(c) normal approximation
covNames <- names(dat)[2:length(names(dat))]
nPara <- ncol(x)

# prior
mu <- as.vector(rep(0,nPara)) # Prior mean vector
Sigma <- tau^2*diag(nPara)

# Log Posterior
LogPostProbit <- function(betaVect,y,X,mu,Sigma){
  
  nPara <- length(betaVect)
  linPred <- X%*%betaVect
  
  # log-likelihood of Probit Regression                                    
  logLik <- sum(y*pnorm(linPred, log.p=T, lower.tail=T) +     #lower.tail=T: P[X ≤ x] 
                  (1-y)*pnorm(linPred,log.p=T, lower.tail=F)) #lower.tail=F: P[X > x]
  
  # prior, library(mvtnorm)
  logPrior <- dmvnorm(betaVect, matrix(0,nPara,1), Sigma, log=TRUE)
  
  # add the log prior and log-likelihood together to get log posterior
  return(logLik + logPrior)
}

initVal <- as.vector(rep(0,ncol(x)))
OptimResults<-optim(initVal,LogPostProbit,gr=NULL,y,x,mu,Sigma,method=c("BFGS"),control=list(fnscale=-1),hessian=TRUE)

# Printing the results to the screen
postMode <- OptimResults$par
postCov <- -solve(OptimResults$hessian) # Posterior covariance matrix is -inv(Hessian)
names(postMode) <- covNames # Naming the coefficient by covariates
approxPostStd <- sqrt(diag(postCov)) # Computing approximate standard deviations.
names(approxPostStd) <- covNames # Naming the coefficient by covariates


# Compare with the results from 2(b).
par(mfrow = c(3,3))
for(i in 1:length(postMode)){
  # normal approximation
  sd_i <- approxPostStd[i]
  betaSeq <- seq(postMode[i] - 4*sd_i,
                 postMode[i] + 4*sd_i,
                 length = 1000)
  plot(betaSeq,dnorm(x = betaSeq,mean = postMode[i],sd = sd_i),
       type = "l", main = names(postMode)[i],col="blue",
       ylab = "",xlab = expression(beta))
  
  # gibbs
  gibbsDensity <- density(res$beta[,i])
  lines(x = gibbsDensity$x, y = gibbsDensity$y , col = "red")
  
  legend("topleft",legend = c("Normal Approx","Gibbs"),
         col = c("blue", "red"),lty = c(1,1),bty = "n",cex = 0.6)
}
par(mfrow = c(1,1))

