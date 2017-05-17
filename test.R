###
### 1. Bernoulli
### 1(a)
a = 14+2
b = 20-14+2
ture_mean = a/(a+b)
# [1] 0.6666667
ture_var = a*b / ((a+b)^2*(a+b+1))
ture_sd = sqrt(ture_var)
# [1] 0.0942809

n = 1:1000
pos_mean = c()
pos_sd = c()
for(i in 1:1000){
  rand = rbeta(i, 16, 8)
  pos_mean[i] = mean(rand)
  pos_sd[i] = sd(rand)
}

plot(x=n, y=pos_mean, type="l", col="blue", ylim = c(0,1),
     main="Posterior Mean & SD")
lines(x=n, y=pos_sd, col="red")
abline(h = ture_mean, lty = 2, col="blue")
abline(h = ture_sd, lty = 2, col = "red")


### 1(b)
set.seed(1234)
# simulation (nDraws = 10000)
theta = rbeta(10000, 16, 8)
pr = mean(as.numeric(theta<0.4))
pr
# [1] 0.0038

# exact value
pbeta(0.4,16,8)
# [1] 0.003972681

### 1(c)
theta = rbeta(10000, 16, 8)
log_odds = log(theta/(1-theta))
hist(log_odds, probability = TRUE, breaks = 30)
lines(density(log_odds), col = "red")


### 2. Log-normal distribution and the Gini coefficient.
### 2(a)
draws = c()
for(i in 1:10000){
  y = c(14, 25, 45, 25, 30, 33, 19, 50, 34, 67)
  n = length(y)
  tao2 = sum((log(y)-3.5)^2) / n
  x = rchisq(1,df=n)
  draws[i] = n*tao2 / x

}
hist(draws, breaks = 50, probability = TRUE, 
     xlab = expression(sigma^2))

# Inverse chi-square posterior distribution
# library(LaplacesDemon)
# hist(rinvchisq(10000, df=10), breaks = 80)

# Theoretical Scaled inverse chi-squared distribution
ScaledInChisq <- function(x,df,tao2){
  (((tao2*df/2)^(df/2)) / (gamma(df/2))) * 
    ((exp(-(df*tao2)/(2*x))) / (x^(1+df/2)))
}
lines(x = seq(0,1.5,0.001),
      y = ScaledInChisq(x=seq(0,1.5,0.001),df = 10,
                        tao2 = tao2),col = "red",type = "l")

### 2(b)
x = sqrt(draws/2)
gini = 2*pnorm(x)-1 # phi function
hist(gini, breaks = 30, probability = TRUE)

### 2(c) 
# Approximate 95% credible interval
# CI = c(mean(gini)-1.96*sd(gini), mean(gini)+1.96*sd(gini))
# [1] 0.1432170 0.3898356

# 95% credible interval 
ci = quantile(gini, probs = c(0.025,0.975))
#  2.5%      97.5% 
#  0.1759851 0.4179913 

names(ci) <- NULL

#  95% Highest Posterior Density interval
HPDcalcCI <- function(x, CI = 0.95){
  densityData <- density(x)
  dens <- densityData$y/sum(densityData$y)
  x <- densityData$x
  while(sum(dens) > CI){
    idx_temp <- which.min(dens)
    dens <- dens[-idx_temp]
    x <- x[-idx_temp]
  }
  out <- c(min(x),max(x))
  return(out)
}

HPDcalcCI(gini)
# [1] 0.1615 0.3934

hist(gini,breaks = 50,xlab = "Gini coefficient",probability = TRUE)
abline(v = HPDcalcCI(gini), col = "red")
abline(v = ci, col = "blue")
legend("topright",legend = c("Equal Tail", "HPD"),
       col = c("blue","red"),lty = c(1,1),bty = "n")


### 3(a)
y = c(-2.44,2.14,2.54,1.83,2.02,2.33,-2.79,2.23,2.07,2.02)
mu = 2.39
#k = rexp(1,1)

posterior <- function(k){
  n = length(y)
  res = exp(sum(k*cos(y-mu))) / (besselI(k,nu=0))^n * exp(-k)
  return(res) 
}

k = seq(0,10,0.1)
post = sapply(k, posterior) 
# nomalize
plot(k, post/sum(post), type = "l", main="Posterior Distribution of K")

# 3b
k[which.max(post)]


