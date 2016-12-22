ll2 <- c()
for(i in 1:N){
  ll2[i] = log_likelihood(n=6, theta = theta[i], x = df$Length[1:6])
}
ll2

# value of theta on maximum likelihood
index_max_y2 <- which.max(ll2)
theta_optimal2 <- theta[index_max_y2]
theta_optimal2

# Plot the curve showing the dependence of log-likelihood on theta
# put the two log-likelihood curves (from step 2 and 3) in the same plot
plot(x=theta,y=ll,col = "red", type = "l",
     xlab = "theta", ylab = "log-likelihood", 
     xlim = c(0.1,5), ylim = c(-150, 0))
lines(x=theta,y=ll2,col = "blue")
