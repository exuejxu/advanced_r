#
### 1. Linear and polynomial regression
d = read.table("C:/Users/Sam/Desktop/732A91 Bayesian Learning/Module 2 - Bayesian Regression and Classification/TempLinkoping2016.txt",
               header = T)

### 1.a Determining the prior distribution of the model parameter.
# fits a quadratic model using ordinary least squares
# preliminary exploratory step
fit = lm(temp ~ time + I(time^2), data=d)

beta_lm = fit$coefficients
# (Intercept)        time   I(time^2) 
#   -10.67535    93.59796   -85.83109 
names(beta_lm) <- NULL
# [1] -10.67535  93.59796 -85.83109

X = matrix(c(d$time, (d$time)^2), ncol=2)
X = cbind(1,X)
Y = matrix(d$temp,ncol = 1)
n = nrow(d)

mu0 <- beta_lm #lm function result
omega0 <- diag(length(mu0)) #identity diagonal matrix
nu0 <- 2
sigma02 <- 6

### 1.b Check if your prior from a) is sensible.
# simulate draws from the joint prior
plot(d$time,d$temp,cex=0.6,pch=16,xlab="Time",ylab="Temp")

library(mvtnorm)
rbeta = matrix(0,nrow=5000, ncol=length(mu0))
for(i in 1:5000){
  # simulate posterior sigma2
  temp <- rchisq(n=1, df=nu0) #chi-square
  rsigma2 <- (nu0*sigma02) / temp #inversed chi-square
  
  # simulate posterior beta
  rbeta[i,] = rmvnorm(n=1, mean=mu0, sigma=rsigma2*solve(omega0))
  
  yhat = X %*% rbeta[i, ]
  lines(d$time,yhat,col=rgb(red=0.2, green=0.2, blue=1.0, 
                            alpha=0.02))
}

beta_hat = colMeans(rbeta)
# [1] -10.60961  93.55857 -85.69468


### 1.c simulates from the joint posterior distribution
mu_n = solve(t(X)%*%X + omega0) %*% ((t(X)%*%X)%*%beta_hat + omega0%*%mu0)
omega_n = t(X)%*%X + omega0
n = nrow(d)
nu_n = nu0 + n
sigma_n2 = (nu0*sigma02 + (t(Y)%*%Y+t(mu0)%*%omega0%*%mu0 - 
                           t(mu_n)%*%omega_n%*%mu_n)) / nu_n

rbeta_pos = matrix(0,nrow=5000, ncol=length(mu0))
for(i in 1:5000){
  # simulate posterior sigma2
  temp <- rchisq(n=1, df=nu_n) #chi-square
  rsigma2_p <- as.numeric((nu_n*sigma_n2) / temp) #inversed chi-square
  
  # simulate posterior beta
  rbeta_pos[i,] = rmvnorm(n=1, mean=mu_n, sigma=rsigma2_p*solve(omega_n))
}
# estimate beta
beta_hat_pos = colMeans(rbeta_pos)

# posterior credible interval
# lower 5% and upper 95%
CI = list()
for(j in 1:ncol(rbeta_pos)){ #3
  CI[[j]]=quantile(rbeta_pos[,j], probs = c(0.025, 0.975))
}
CI
#[[1]]
#5%       95% 
#  -11.42643  -9.79097  
#[[2]]
#5%      95% 
#  90.32303 96.86747 
#[[3]]
#5%       95% 
#  -88.94770 -82.55349 
beta_lower <- as.vector(c(CI[[1]][1],CI[[2]][1],CI[[3]][1]))
beta_upper <- as.vector(c(CI[[1]][2],CI[[2]][2],CI[[3]][2]))

plot(d$time,d$temp,cex=0.6,pch=16,xlab="Time",ylab="Temp")
lines(d$time, y=X%*%beta_hat_pos, col="red")
lines(d$time, y=X%*%beta_lower, col="blue")
lines(d$time, y=X%*%beta_upper, col="blue")


### 1.d locate the time with the highest expected temp
# derivative y by x = 0 
xhat = -beta_hat_pos[2]/ (2*beta_hat_pos[3]) #use estimated beta
# [1] 0.5458163
#use all simulations, result is same.
xhat = mean(-rbeta_pos[,2]/ (2*rbeta_pos[,3])) 
# [1] 0.5458492

### 1.e


### 2.Posterior approximation for classification with logistic regression
dat = read.table("C:/Users/Sam/Desktop/732A91 Bayesian Learning/Module 2 - Bayesian Regression and Classification/WomenWork.dat",
               header = T)

y = as.vector(dat$Work)
x = as.matrix(dat[,-1]) #already have!! 1st col: constant 
#### if not: x = cbind(1, x)

### 2.a & b logistic regression
# added a zero in the model so that R doesn’t add an extra intercept
# (we already have an intercept term from the Constant feature).
glmModel <- glm(Work ~ 0 + ., data = dat, family = binomial)

# Prior: scaling factor such that Prior Covariance = (tau^2)*I
tau <- 10
covNames <- names(dat)[2:length(names(dat))]
nPara <- ncol(x) #8

# Setting up the prior
mu <- as.vector(rep(0,nPara)) # Prior mean vector
Sigma <- tau^2*diag(nPara)

# Defining the functions that returns the log posterior (Logistic models). 
# 1ST input argument of this function must be the one that we optimize on,
# i.e. the regression coefficients.

LogPostLogistic <- function(betaVect,y,X,mu,Sigma){
  
  nPara <- length(betaVect);
  linPred <- X%*%betaVect;
  
  # evaluating the log-likelihood                                    
  logLik <- sum( linPred*y -log(1 + exp(linPred)))
  if (abs(logLik) == Inf) logLik = -20000 
  # Likelihood is not finite, stear the optimizer away from here!
  
  # evaluating the prior, library(mvtnorm)
  logPrior <- dmvnorm(betaVect, matrix(0,nPara,1), Sigma, log=TRUE)
  
  # add the log prior and log-likelihood together to get log posterior
  return(logLik + logPrior)
}

initVal <- as.vector(rep(0,ncol(x))); 
OptimResults<-optim(initVal,LogPostLogistic,gr=NULL,y,x,mu,Sigma,method=c("BFGS"),control=list(fnscale=-1),hessian=TRUE)
OptimResults

# Printing the results to the screen
postMode <- OptimResults$par
postCov <- -solve(OptimResults$hessian) # Posterior covariance matrix is -inv(Hessian)
names(postMode) <- covNames # Naming the coefficient by covariates
approxPostStd <- sqrt(diag(postCov)) # Computing approximate standard deviations.
names(approxPostStd) <- covNames # Naming the coefficient by covariates

# approximate 95% credible interval 
# for the variable NSmallChild.
sd <- sqrt(diag(postCov)[which(covNames == "NSmallChild")])
CI <- c(postMode["NSmallChild"]-1.96*sd, 
        postMode["NSmallChild"]+1.96*sd)
#-2.121425, -0.5968414 

# plot CI
seqc <- seq(postMode["NSmallChild"] - 4*sd,
            postMode["NSmallChild"] + 4*sd,
            length = 1000)
plot(seqc,dnorm(x = seqc,mean = postMode["NSmallChild"],sd = sd),type = "l",
     main = "NSmallChild",xlab = expression(beta[NSmallChild]),ylab="")
abline(v = CI,lty = 2,col = "red")


### 2.c simulates from the predictive distribution of the response variable in a logistic regression. 
x_test = matrix(c(1,10,8,10,(10/10)^2,40,1,1),nrow = 1)
y_pred = c()
prob = c()
for(i in 1:1000){
  # Use your normal approximation to simulate beta vector
  # library(mvtnorm)
  sam_beta = as.vector(rmvnorm(1,mean=OptimResults$par, #as.vector()
                               sigma = -solve(OptimResults$hessian)))
    
  temp = exp(x_test%*%sam_beta) # should be a number!!!
  prob[i] = temp / (1+temp) 
  y_pred[i] = rbinom(n = 1, size = 1, prob = prob[i])
}

mean(prob)
# predictive distribution for the Work
hist(y_pred,breaks=30,xlab="Work",probability = TRUE,
     main="Predictive distribution for the Work variable")

################# same result
#> x[1:2,]%*%t(b)
#[,1] -0.2279402
#[2,] -0.6140388
#> x[1:2,]%*%as.vector(b)
#[1,] -0.2279402
#[2,] -0.6140388
