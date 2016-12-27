####### Assignment 1. Uncertainty estimation
# sep = ";", read.csv2
mydata <- read.csv2("C:/Users/Sam/Desktop/machine learning/lab4/State.csv")

### 1.1 Reorder data with respect to the increase of MET
ind <- order(mydata$MET,decreasing = FALSE)
for(i in 1:ncol(mydata)){
  mydata[, i] = mydata[, i][ind]
}
mydata

# Plot MET versus EX
plot(mydata$MET, mydata$EX, col = "blue", xlab = "MET", ylab = "EX")

### 1.2 Fit a regression tree model with target EX~feature MET 
# the number of the leaves is selected by cross-validation #######

# Selecting optimal tree by penalizing
# use the entire data set, nrow(mydata)=48
# set minimum number of observations in a leaf = 8(setting minsize in tree.control)
library(tree)
fit = tree(EX~MET, data = mydata, control = tree.control(nobs = 48, minsize = 8))
predict(fit)
set.seed(12345)
cv.res = cv.tree(fit) #cross-validation
plot(cv.res$size, cv.res$dev, type = "b", col = "red")
plot((cv.res$k), cv.res$dev, type = "b", col = "red")
plot(cv.res)

# Report optimal tree
# gives the deviance for each K (small is better)
cv.res$dev
# [1] 192109.0 186515.5 188981.5
index <- which.min(cv.res$dev) #5
bestsize <- cv.res$size[index]
bestsize  # 3

# refit the model with the number of leafs of best size
finalTree = prune.tree(fit, best = bestsize) ##########choose from above plot, min deviance
Ffit = predict(finalTree)
plot(finalTree)
text (finalTree , pretty =0)

# Plot the original and the fitted data
plot(mydata$MET, mydata$EX, col = "blue", xlab = "MET", ylab = "EX")
points(mydata$MET, Ffit, col = "red")

# Plot histogram of residuals.  
residuals = residuals(finalTree)
hist(residuals)
