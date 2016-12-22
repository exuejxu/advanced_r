####### Assignment 2 Using GAM to examine the mortality rates
mydata = read.csv2("C:/Users/Sam/Desktop/machine learning/machine learning lab/influenza.csv", sep = ";", dec = ",")

### 2.1 time series plots: use Time as X 
# mortality and influenza number vary with time 
plot(mydata$Time, mydata$Mortality, type = "l", col = "orange", 
     main = "Mortality number vs time", xlab = "Time", ylab = "Mortality" )
plot(mydata$Time, mydata$Influenza, type = "l", col = "blue", 
     main = "Influenza number vs time", xlab = "Time", ylab = "Influenza number" )

### 2.2 GAM model, from mgcv package, Fit Generalized Additive Models
library(mgcv)

# s(k, sp)
# sp -  smoothing penalty
# k no. of unique values of variable in smoothing splines, always specify this number
k = length(unique(mydata$Week))  #52

# Fit GAM model, a gaussian model 
# which has linear terms in Year and spline term in Week by using generalized cross-validation(default fun in gam)
model = gam(Mortality~Year+s(Week, k = 52), family = gaussian, data = mydata)
summary(model)

### 2.3 Plot predicted & observed mortality against time
plot(mydata$Time, mydata$Mortality, type = "l", col = "orange", 
    xlab = "Time", ylab = "Mortality" )
yhat = predict(model)
lines(mydata$Time, yhat, col ="blue")
legend("topright",col=c("orange","blue"),pch=1,legend=c("observed","predicted"))

SSE=sum((yhat-mydata$Mortality)^2)
SSE

# Plot the spline component
library(rgl)
library(akima)
s=interp(mydata$Year,mydata$Week, fitted(model))
persp3d(s$x, s$y, s$z, col="red")

# Seeing trend and seasonal pattern
plot(model)

### 2.4 Examine how the penalty factor of the spline function influences the deviance of the model.
# plot predicted & observed mortality against time 
# for cases of very high and very low penalty factors.

# high penalty sp=100
model1 = gam(Mortality~Year+s(Week, k = 52, sp=100), data = mydata)
plot(mydata$Time, mydata$Mortality, type = "l", col = "orange", 
     xlab = "Time", ylab = "Mortality", main = "High penalty")
yhat1 = predict(model1)
lines(mydata$Time, yhat1, col ="blue")
legend("topright",col=c("orange","blue"),pch=1,legend=c("observed","predicted"))

summary(model1)

# low penalty sp=0.1
model2 = gam(Mortality~Year+s(Week, k = 52, sp=0.1), data = mydata)
plot(mydata$Time, mydata$Mortality, type = "l", col = "orange", 
     xlab = "Time", ylab = "Mortality", main = "Low penalty"  )
yhat2 = predict(model2)
lines(mydata$Time, yhat2, col ="blue")
legend("topright",col=c("orange","blue"),pch=1,legend=c("observed","predicted"))

summary(model2)

### 2.5  Plot the residuals(from model)& influenza values against time 
# (in one plot)

# k no. of unique values of variable in smoothing splines
k = length(unique(mydata$Week))  #52

# Fit GAM model, a gaussian model 
# which has linear terms in Year and spline term in Week by using generalized cross-validation
model = gam(Mortality~Year+s(Week, k = 52), family = gaussian, data = mydata)

plot(mydata$Time, model$residuals, type = "l", col = "light green", 
     xlab = "Time", ylab = "Influenza/ Residuals" )
lines(mydata$Time, mydata$Influenza, xlab = "Time", col = "blue")
legend("topright",col=c("light green","blue"),pch=1,legend=c("residuals","influenza"))

### 2.6 GAM model
# mortality is described as spline functions of year week, & no.of influenza.
k = length(unique(mydata$Week))  #52 
k1 = length(unique(mydata$Year)) #9
k2 = length(unique(mydata$Influenza)) #85
 
model3 = gam(Mortality ~ s(Year, k = 9) + s(Week, k = 52) + s(Influenza, k = 85), data = mydata)
summary(model3)
yhat3 = predict(model3)

# test if mortality is influenced by influenza.
# if 2 plots have diff ylim, modify ylim, choose c(min, max)
plot(mydata$Time, yhat3,type = "l", ylim = c(0,3000), col ="red",
     xlab = "Time", ylab = "Mortality/ Influenza")
lines(mydata$Time, mydata$Influenza, col ="blue")
legend("topright",col=c("red","blue"),pch=1,legend=c("Mortality","Influenza"))

# Compute SSE for this fit.
SSE = sum((mydata$Mortality-yhat3)^2)
SSE

# plot original & fitted Mortality against Time
plot(mydata$Time, mydata$Mortality, type = "l", col = "pink", 
     xlab = "Time", ylab = "Mortality")
yhat3 = predict(model3)
lines(mydata$Time, yhat3, col ="blue")
legend("topright",col=c("orange","blue"),pch=1,legend=c("observed","predicted"))

