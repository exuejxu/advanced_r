###### Implement the EM algorithm 
###### for mixtures of multivariate Benouilli distributions.


set.seed(1234567890)

# min change in log likelihood between two consecutive EM iterations
min_change <- 0.1 
N=1000 # number of training points
D=10 # number of dimensions
x <- matrix(nrow=N, ncol=D) # training data
true_pi <- vector(length = 3) # true mixing coefficients
true_mu <- matrix(nrow=3, ncol=D) # true conditional distributions
true_pi=c(1/3, 1/3, 1/3)
true_mu[1,]=c(0.5,0.6,0.4,0.7,0.3,0.8,0.2,0.9,0.1,1)
true_mu[2,]=c(0.5,0.4,0.6,0.3,0.7,0.2,0.8,0.1,0.9,0)
true_mu[3,]=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
plot(true_mu[1,], type="o", col="blue", ylim=c(0,1))
points(true_mu[2,], type="o", col="red")
points(true_mu[3,], type="o", col="green")
# Producing the training data
for(n in 1:N) {
  k <- sample(1:3,1,prob=true_pi)
  for(d in 1:D) {
    x[n,d] <- rbinom(1,1,true_mu[k,d])
  }
}
K=3 # number of guessed components
z <- matrix(nrow=N, ncol=K) # fractional component assignments
pi <- vector(length = K) # mixing coefficients
mu <- matrix(nrow=K, ncol=D) # conditional distributions

# Random initialization of the paramters
pi <- runif(K,0.49,0.51)
pi <- pi / sum(pi)
for(k in 1:K) {
  mu[k,] <- runif(D,0.49,0.51)
}
pi
mu

pi_cur <- pi
mu_cur <- mu

# max number of EM iterations
max_it <- 100
# log likelihood of the EM iterations
llik <- vector(length = max_it) # log likelihood of the EM iterations

for(it in 1:max_it) {
  plot(mu_cur[1,], type="o", col="blue", ylim=c(0,1))
  points(mu_cur[2,], type="o", col="red")
  points(mu_cur[3,], type="o", col="green")
  #points(mu[4,], type="o", col="yellow")
  Sys.sleep(0.5)
  
  # E-step: Computation of the fractional component assignments
  
  z <- list()
  for(k in 1:3){
    r1 = matrix(0, nrow = 1000, ncol=10)
    for(n in 1:1000){
      for(i in 1:10){
        r1[n,i] =  mu_cur[k,i]^x[n, i] * (1-mu_cur[k,i])^(1-x[n, i])
      }
    }
    #View(r1)
    r11=1
    for(i in 1:10){
      r11 = r11 * r1[,i]
    }
    r11
    #print(length(r11))
    z[[k]] = pi_cur[k] * r11
  }
  z
  zn1 = z[[1]]/ (z[[1]] + z[[2]] + z[[3]])
  zn2 = z[[2]]/ (z[[1]] + z[[2]] + z[[3]])
  zn3 = z[[3]]/ (z[[1]] + z[[2]] + z[[3]])
  zn = list(zn1, zn2, zn3)
  
  
  #Log likelihood computation.
  
  l1 = matrix(0, nrow = 1000, ncol=10)
  l3 = c()
  for(k in 1:3){
    for(n in 1:1000){
      for(i in 1:10){
        l1[n,i] = x[n,i]*log(mu_cur[k,i]) + (1-x[n,i])*log((1-mu_cur[k,i]))
      }
    }
    #View(l1)
    # sum up all the dimension of D(i)
    l2 = 0
    for(i in 1:10){
      l2 = l2 + l1[,i]
    }
    #print(l2)
    l3[k] = log(pi[k]) + sum(l2*zn[[k]])
  }
  l3
  llik[it]= sum(l3)
  
  cat("iteration: ", it, "log likelihood: ", llik[it], "\n")
  flush.console()
  # Stop if the lok likelihood has not changed significantly
  if((it > 1) && (abs(llik[(it-1)]-llik[it]) <= 0.1)) break
  
  #M-step: ML parameter estimation from the data and fractional component assignments
  # compute pi
  pi_cur = c()
  for(k in 1:3){
    pi_cur[k] = mean(zn[[k]])
  }
  pi_cur
  
  # compute mu
  mu_cur = matrix(0, ncol = 10, nrow = 3)
  for(k in 1:3){
    for(i in 1:10){
      mu_cur[k,i] = sum(zn[[k]] * x[,i])/ sum(zn[[k]])
    }
  }
  mu_cur
  
  
}
pi_cur
mu_cur
plot(llik[1:it], type="o")
