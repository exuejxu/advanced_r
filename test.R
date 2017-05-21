#
# posterior 95% equal tail credible interval
# lower 2.5% and upper 97.5%

# dim(curves) [366,5000]
curves <- sapply( 1:nrow(rbeta_pos), function (i) X %*% rbeta_pos[i,] ) 
# length(meancurve) 366
meancurve <- apply(curves, 1, mean)

# compute CI for every value of time
# then connect the lower and upper limits of the interval by curves
# dim(intervals) 2 366
intervals <- apply(curves, 1, function(x){
  quantile(x, probs = c(0.025, 0.975)) })

plot(d$time,d$temp,cex=0.6,pch=16,xlab="Time",ylab="Temp")
lines(d$time, y=X%*%beta_hat_pos, col="red") #mean
# lines(d$time, y=meancurve, col="red")      #same as above
lines(d$time, y=intervals[1,], col="blue")   #lower
lines(d$time, y=intervals[2,], col="blue")   #upper

