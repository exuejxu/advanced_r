#### 2.4 log-likelihood fun for Bayesian model with a prior
theta <- seq(from = 0.1, to = 5, by=0.1)
n = nrow(df)
N = length(theta)
lamda = 10

l_theta <- function(n, theta, x, lamda){
  l_theta <- (n*log(theta) - theta*sum(x) + log(lamda*exp(-lamda*theta)))
  return(l_theta)
}

# loop in a given theta
ll3 <- c()
for(i in 1:N){
  ll3[i] = l_theta(n=n, theta = theta[i], x = df$Length, lamda)
}
ll3

# Plot the curve showing the dependence of l(theta)
plot(x= theta, y=ll3,col = "green", type = "l", xlab = "theta", 
     ylab = "l(theta)", xlim = c(0.1,5), ylim = c(-150, -20))

# value of theta on maximum likelihood
index_max_y3 <- which.max(ll3)
theta_optimal3 <- theta[index_max_y3]
theta_optimal3

#### 2.5 use theta value found in step 2 to generate 50 new observations
# use standard random number generators,random exponential distribution
random = rexp(50, rate = theta_optimal1)

# Create the histograms of the original and the new data
hist(random, xlab = "Lifetime", main = "Histogram of new data")
hist(df$Length, xlab = "Lifetime", main = "Histogram of original data")

