# Use knearest()
fit7 = knearest(traindata, k=5, testdata)
true7 = testdata[,ncol(data)]

sens7 = c()
spc7 = c()
pi = seq(from = 0.05, to = 0.95, by = 0.05)
for (i in 1:length(pi)){
  prediction7 = as.integer(fit7>pi[i])
  confusion7 = table(true7, prediction7)
  sens7[i] = confusion7[2,2]/sum(confusion7[2,])
  spc7[i] = confusion7[1,1]/sum(confusion7[1,])
}
sens7
spc7

# use kknn()
library(kknn)
fit8 = kknn(as.factor(train$Spam)~.,train,test, k = 5)

prob8 = fit8$prob # probability %
true8 = testdata[, ncol(data)]

sens8 = c()
spc8 = c()
pi = seq(from = 0.05, to = 0.95, by = 0.05)
for (i in 1:length(pi)){
  prediction8 = as.integer(prob8[,2]>pi[i])  # classify(0,1)
  confusion8 = table(true8, prediction8)
  sens8[i] = confusion8[2,2]/sum(confusion8[2,])
  spc8[i] = confusion8[1,1]/sum(confusion8[1,])
}
sens8
spc8

# plot the corresponding ROC curves of 2 methods in 1 plot
x1  <- 1 - spc7 # FPR
y1 <- sens7     # TPR
x2 <- 1 - spc8
y2 <- sens8

# 1st plot
plot(x=x1, y=y1,col = "red", type = "l",  # type"l"for line
     xlab = "fpr", ylab = "tpr", 
     xlim = c(0, 1), ylim = c(0, 1))  #usually x,ylim=c(0,1) for probility
# 2nd plot
lines(x=x2, y=y2,col = "blue", type = "l")
