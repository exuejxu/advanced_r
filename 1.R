# Import data and plot a scatter chart of LMR versus day
mydata = read.csv2("mortality_rate.csv", sep = ",", dec = ".")
mydata$LMR = log(mydata$Rate)
plot(mydata$Day, mydata$LMR, col = "blue", xlab = "Day", ylab = "LMR")

# Implement a function performing NW kernel smoothing with Epanechnikov kernel
NWksmoothing = function(X, Y, Xtest, lambda){
 
  n = length(Xtest)
  pref = numeric(n)
  for (i in 1:n){
    dis = abs(X-Xtest[i])
    Kl = numeric(length(dis))
    for (j in 1: length(dis)){
      if (dis[j]/lambda<1){
        Kl[j] =3/4*(1-(dis[j]^2)/(lambda^2))
      }else{
        Kl[j] = 0
      }
           
    }
    pref[i] = sum(Kl*Y)/sum(Kl)
  }
  return(pref)
}

# Test the function 
minR = min(mydata$Day)
maxR = max(mydata$Day)
Xtest = seq(from = minR, to = maxR, by = 0.1)
lambda = 10
Ytest = NWksmoothing(mydata$Day, mydata$LMR, Xtest, lambda)
plot(mydata$Day, mydata$LMR, col = "blue", xlab = "Day", ylab = "LMR")
points(Xtest, Ytest, pch = 20, col = "red")
Y = NWksmoothing(mydata$Day, mydata$LMR, mydata$Day, lambda = 10)
MSE = mean((Y-mydata$LMR)^2)

# Fit LMR using SVM for regression with RBF kernel
library(kernlab)
set.seed(12345)
kfit = ksvm(LMR~Day, data = mydata, kernel = "rbfdot", epsilon = 0.01)
Y1 = predict(kfit, mydata)
MSE1 = mean((Y1-mydata$LMR)^2)
ndata = data.frame(Day = Xtest)
Y2 = predict(kfit, newdata = ndata)
plot(mydata$Day, mydata$LMR, col = "blue", xlab = "Day", ylab = "LMR")
points(Xtest, Y2, pch = 20, col = "red")

# Normal kernel in fancova
library(fANCOVA)
x = mydata$Day
y = mydata$LMR
# generalized cross-validation (gcv)
fit1 = loess.as(x,y, degree = 1, criterion = "gcv", family = "gaussian" )
summary(fit1)
Yf = predict(fit1, Xtest, se = TRUE)
plot(mydata$Day, mydata$LMR, col = "blue", xlab = "Day", ylab = "LMR")
points(Xtest, Yf$fit, pch = 20, col = "red")
# Confidence band
points(Xtest, Yf$fit+2*Yf$se.fit, pch = ".", col = "green")
points(Xtest, Yf$fit-2*Yf$se.fit, pch = ".", col = "green")
