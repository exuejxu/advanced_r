### 1.3 Compute and plot the 95%(default) confidence bands for the regression tree model from step 2
# non-parametric bootstrap.
library(boot)
#reordering data according to feature:MET
data2=mydata[order(mydata$MET),]

# computing bootstrap samples
f=function(data, ind){
  # extract bootstrap sample
  data1=data[ind,]
  # fit regression tree model
  fit = tree(EX~MET, data = data1, control = tree.control(nobs = 48, minsize = 8))
  res = prune.tree(fit, best = bestsize)  
  #predict values for all Area values from the original data
  EXP=predict(res,newdata=data2) 
  return(EXP)
}

# make bootstrap
res=boot(data2, f, R=2000) 

# Bootstrap cofidence bands for linear model
# compute confidence bands
e=envelope(res)

# compute fitted line
fit2=tree(EX~MET, data=data2, control = tree.control(nobs = 48, minsize = 8)) 
fianlTree = prune.tree(fit2, best = bestsize) #######
EXP=predict(fianlTree)

#plot fitted line
plot(mydata$MET, mydata$EX, pch=21, bg="orange", xlab = "MET", ylab = "EX")
points(data2$MET,EXP,type="l") 

# plot cofidence bands
points(data2$MET,e$point[2,], type="l", col="blue")
points(data2$MET,e$point[1,], type="l", col="blue")


### 1.4 Compute and plot the 95% confidence bands && prediction bands
# by using parametric bootstrap.
# assume Y~N(mu, s^2)
library(boot)
#reordering data according to feature:MET
data2=mydata[order(mydata$MET),]
fit2 = tree(EX~MET, data = data2, control = tree.control(nobs = 48, minsize = 8))
mle = prune.tree(fit2, best = bestsize) #####

rng=function(data, mle) {
  data1=data.frame(EX=mydata$EX, MET=mydata$MET)
  n=length(mydata$EX)
  #generate new target:EX
  data1$EX=rnorm(n,predict(mle, newdata=data1),sd(residuals(mle)))
  return(data1)
}

f1=function(data1){
  #fit regression tree model
  res=tree(EX~MET, data= data1, control = tree.control(nobs = 48, minsize = 8))
  res = prune.tree(res, best = bestsize) ######
  #predict values for all Area values from the original data
  EXP=predict(res,newdata=data2) 
  return(EXP)
}
# make bootstrap
res2=boot(data2, statistic=f1, R=1000, mle=mle, ran.gen=rng, sim="parametric")

# Bootstrap cofidence bands for tree model
# compute confidence bands
e2=envelope(res2)

# compute fitted line
fit2=tree(EX~MET, data=data2, control = tree.control(nobs = 48, minsize = 8)) 
fianlTree = prune.tree(fit2, best = bestsize) #######
EXP=predict(fianlTree)

# plot fitted line
plot(mydata$MET, mydata$EX, pch=21, bg="orange", xlab = "MET", ylab = "EX",
     ylim = c(100,450), xlim = c(0,90))
points(data2$MET,EXP,type="l") 

# plot cofidence bands
points(data2$MET,e2$point[2,], type="l", col="blue")
points(data2$MET,e2$point[1,], type="l", col="blue")

# compute Prediction bands
data2=mydata[order(mydata$MET),]
fit2 = tree(EX~MET, data = data2, control = tree.control(nobs = 48, minsize = 8))
mle = prune.tree(fit2, best = bestsize)

f2 = function(data1){
  #fit model
  temp = tree(EX~MET, data = data1, control = tree.control(nobs = 48, minsize = 8))
  res = prune.tree(temp, best = bestsize) #####
  #predict values for all Area values from the original data
  EXP = predict(res, newdata = data2)
  n = length(data2$EX)
  predictedP = rnorm(n, EXP, sd(residuals(mle)))
  return(predictedP)
  
}
# R=2000, more number
res3 = boot(data2, statistic = f2, R = 2000, mle = mle, ran.gen = rng, sim = "parametric") 

e3 = envelope(res3)

# Plot prediction bands
points(data2$MET, e3$point[2,], type = "l", col = "green")
points(data2$MET, e3$point[1,], type = "l", col = "green")


