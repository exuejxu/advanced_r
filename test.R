### Question 1: Genetic algorithm
### 1.1 Define the function
f <- function(x) {
  x^2 / exp(x) - 2*exp((-1) * (9*sin(x)) / (x^2 + x + 1))
}

### 1.2 Define the function crossover()
crossover <- function(x, y) {
  (x + y) / 2
}

### 1.3 Define the function mutate()
# modulus (x mod y) 5%%2 is 1
# x %/% y	integer division 5%/%2 is 2
mutate <- function(x) {
  x^2 %% 30
}

### 1.4
myGenetic <- function(maxiter, mutprob){
  ### (a)(b)(c)
  # Plot f(x)
  plot(seq(0,30,0.01), f(seq(0,30,0.01)), type = "l", 
       xlab = "x", ylab = "f(x)") 
  # Defines an initial population X
  X = seq(from = 0, to = 30, by = 5)
  points(X, f(X), col = "blue", pch = 20)
  
  # Computes vector Values
  Values = f(X)
  maxV = c()
  ### (d) Performs maxiter iterations
  for(i in 1:maxiter) {
    parents = sample(X, size=2)
    victim = which.min(Values)
    
    kid = crossover(x=parents[1], y=parents[2])
    # Mutate this kid with probability mutprob
    if(runif(1) <= mutprob) kid=mutate(kid)
    # The victim is replaced by the kid in the population 
    X[victim] = kid
    # vector Values is updated.
    Values[victim] = f(kid)
    maxV[i] = max(Values)
  }

  points(X, Values, col = "red", pch=8)
  legend("topright", c("Original", "Current"), 
         lty=c(1,1), cex = 0.8,col = c("blue","red"))
  
  return(list(finalX=X, finalValues=Values, max_value=maxV))
}

myGenetic(10, 0.1)
myGenetic(10, 0.5)
myGenetic(10, 0.9)
myGenetic(100, 0.1)
myGenetic(100, 0.5)
myGenetic(100, 0.9)

### Question 2: EM algorithm
data = read.csv("C:/Users/Sam/Desktop/Computational Statistics/lab6/physical1.csv") 

### 2.1 Make a time series plot
X = data$X
Y = data$Y
Z = data$Z

plot(X, Y, type = "l", col="blue", ylab = "Y / Z")
lines(X, Z, col="red")
legend("topright", c("Y", "Z"), 
       lty=c(1,1), lwd=c(.5,.5),col = c("blue","red"))

### 2.2 2.3  
#Define log-likelihood function
ll <- function(X, Y, Zobs, Xobs, lambda, n){
  2*sum(log(X)) - ( n*log(2) ) - ( 2*n*log(lambda) ) -
    ( sum(Y*X)/lambda ) - ( sum(Zobs*Xobs)/(2*lambda) )
}

EM <- function(X,Y,Z){
  Xobs <- X[!is.na(Z)]
  Xmiss <- X[is.na(Z)]
  Zobs <- Z[!is.na(Z)]
  n <- length(X)
  m <- length(Xmiss)
  #Initial values
  lambdat <- 100

  repeat{
    #M-step
    lambdat1 <- ( sum(Y*X) + (sum(Zobs*Xobs)/2) + (lambdat*m) )/(2*n)
    #Compute log-likelihood using current estmates
    llt <- ll(X, Y, Zobs, Xobs, lambdat1, n)
    #Print current parameter values and likelihood
    cat("lambda =",lambdat1, "log-likelihood =", llt, "\n")
    #Stop if converged
    if(abs(lambdat1 - lambdat) < 0.001) break
    lambdat <- lambdat1
  }
}
EM(data$X, data$Y, data$Z)
#optimal lambda 10.69566

lambda <- 10.69566
X <- data$X
Y <- data$Y
Z <- data$Z
EY <- lambda/X
EZ <- (2*lambda)/X
plot(X, Z, type="l", ylab="")
lines(X, EZ, col="red", lwd=2)
lines(X, Y, col="blue")
lines(X, EY, col="green", lwd=2)
legend("topright", c("Y","Z","EY","EZ"), col=c("blue","black","green","red"), lwd=1)
