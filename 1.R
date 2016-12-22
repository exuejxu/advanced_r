#### Assignment 2. Inference about lifetime of machines
# 2.1 Import the data to R.
df <- read.csv("C:/Users/Sam/Desktop/machine learning/lab1/machines.csv")

#### 2.2
# computes the log-likelihood for a given theta & a given data vector x.
n = nrow(df)

# log-likelihood fun
log_likelihood <- function(n, theta, x){
  log_likelihood <- n*log(theta) - theta * sum(x)
  return(log_likelihood)
}

# loop in a given theta
theta <- seq(from = 0.1, to = 5, by = 0.1)
N = length(theta)

ll <- c()
for(i in 1:N){
  ll[i] = log_likelihood(n=n, theta = theta[i], x = df$Length)
}
ll

# Plot the curve showing the dependence of log-likelihood on theta
plot(x=theta,y=ll,col = "red", type = "l",
     xlab = "theta", ylab = "log-likelihood", 
     xlim = c(0.1,5), ylim = c(-150, -20))

# value of theta on maximum likelihood
index_max_y1 <- which.max(ll)
theta_optimal1 <- theta[index_max_y1]
theta_optimal1

