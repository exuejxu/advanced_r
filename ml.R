
###### Assignment 2
attach(mtcars)
View(mtcars)

# Scale predictors qsec and hp
mtcars$qsec = scale(mtcars$qsec)
mtcars$hp = scale(mtcars$hp)

### 2.1 Plot the data in the coordinates hp versus qsec
# where the data are colored by am.
index = (mtcars$am==0)
plot(mtcars$qsec[index],mtcars$hp[index], col = "red", 
     xlab = "qsec", ylab = "hp")
points(mtcars$qsec[!index], mtcars$hp[!index], col = "blue")


### 2.2 Perform LDA with response am and predictors qsec and hp
### a) Equal priors
library(MASS)
resLDA=lda(am~qsec+hp, data=mtcars, prior=c(.5,.5))
print(resLDA)

Pred=predict(resLDA)
plot(mtcars$qsec, mtcars$hp, col=as.double(Pred$class)+1, pch=21, bg=as.double(Pred$class)+1, main="Prediction") 

# Misclassified items
table(Pred$class, mtcars$am)

### b) Proportional priors
resLDA=lda(am~qsec+hp, data=mtcars, prior=c(.3,.7))

### 2.3 Implement kernel density estimation with Epanechnikov kernel
epan_k = function(X, Y, Xtest, lambda){
  
  n = length(Xtest)
  pref = numeric(n)
  for (i in 1:n){
    dis = abs(X-Xtest[i])
    Kl = numeric(length(dis))
    for (j in 1: length(dis)){
      if (dis[j]/lambda<1){
        Kl[j] = (1-(dis[j]^2)/(lambda^2))
      }else{
        Kl[j] = 0
      }
      
    }
    pref[i] = sum(Kl*Y)/sum(Kl)
  }
  return(pref)
}

# a)
# matrix X
index = (mtcars$am==0)
X = matrix(c(mtcars$qsec[index],mtcars$hp[index]), ncol = 2) 
Xtest = matrix(c(mtcars$qsec,mtcars$hp), ncol = 2) 
lambda = 0.2
# Test the function 
Ytest = epan_k(X, Y=mtcars$am[index], Xtest, lambda)
plot(mtcars$qsec, mtcars$hp, col = "blue", xlab = "qsec", ylab = "hp")
points(Xtest, Ytest, pch = 20, col = "red")

# b)
# matrix X
index = (mtcars$am==1)
X = matrix(c(mtcars$qsec[index],mtcars$hp[index]), ncol = 2) 
Xtest = matrix(c(mtcars$qsec,mtcars$hp), ncol = 2) 
lambda = 0.2
# Test the function 
Ytest = epan_k(X, Y=mtcars$am[index], Xtest, lambda)
plot(mtcars$qsec, mtcars$hp, col = "blue", xlab = "qsec", ylab = "hp")
points(Xtest, Ytest, pch = 20, col = "red")

