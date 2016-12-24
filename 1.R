### 2.3. compute the bagging regression tree
# use the whole dataset
### bagging regression tree.iterations=100
library(tree)
mse3 = c()
for(B in 1:100){
  data_pos <- sample(1:nrow(data), size=nrow(data), replace = T)
  fittree = tree(Bodyfat_percent~., data = data[data_pos,])
  pred = predict(fittree,newdata=data)
  mse = mean((pred-data$Bodyfat_percent)^2)
  mse3 = c(mse3, mse)
} 

mse3 #length 100
hist(mse3)
# upper bound of the squared error
mean(mse3)


####### Assignment 3 boosting regression tree analysis
### 3.1. Interpret the plot resulting from the code below.
library(mboost)

bf=read.csv2("C:/Users/Sam/Desktop/machine learning/Lab2Block2/bodyfatregression.csv")
m=blackboost(Bodyfat_percent~Waist_cm+Weight_kg, data=bf)
mstop(m)
cvf=cv(model.weights(m), type = "kfold")
cvm=cvrisk(m, folds =cvf, grid = 1:100)## Use crossvalidation to find stopping time M
plot(cvm)

### 3.2. Estimate the squared error of the boosting regression tree.
# boost_control defines no. of boosting iterations mstop.
b=blackboost(Bodyfat_percent~Waist_cm+Weight_kg, data=train, 
             control=boost_control(mstop=mstop(cvm)))
pred <- predict (b, newdata = test) 
mse3 = mean((pred-test$Bodyfat_percent)^2)
mse3

####### Assignment 4 evaluate the performance of Adaboost classification trees & random forests
df <- read.csv2("C:/Users/Sam/Desktop/machine learning/Lab2 block2/spambase.csv")

n1=dim(df)[1]
set.seed(1234567890) 
id=sample(1:n1, floor(n1*2/3)) 
train1=df[id,] 
test1=df[-id,] 

### Adaboost classification trees
library(mboost)
# as.factor(Spam)~.
# Specify the loss function corresponding to Adaboost
# parameter: family=AdaExp()
# mstop: an integer giving the number of initial boosting iterations.

ada <- function(nt){
  fitb = blackboost(as.factor(Spam)~., data=train1, family=AdaExp(), 
                    control=boost_control(mstop=nt))   # 0,1,...
  pred <- predict(fitb, test1, type="class")
  t = table(true=test1$Spam, pred=pred)
  mis = 1-sum(diag(t))/sum(t)
  return(mis)
}

# tree number
nt = seq(from=10, to=100, by =10)
mis = sapply(nt, FUN = ada)

mis
ind <- which.min(mis)
nt[ind]
#[1] 80

# plot error rates vs the number of trees
plot(nt, mis, xlab = "Number of trees", ylab = "Error rate", type = "l")



### random forests
# as.factor(Spam)~.
# ntree: number of tree

library(randomForest)

rand <- function(nt){
  fit <- randomForest(as.factor(Spam)~., data=train1, ntree=nt)
  pred <- predict(fit, test1)   # probility %
  t1 = table(true=test1$Spam, pred=pred)
  mis1 = 1-sum(diag(t1))/sum(t1)
  return(mis1)
}

nt = seq(from=10, to=100, by =10)
mis1 = sapply(nt, FUN = rand)
mis1

ind1 <- which.min(mis1)
nt[ind1]
#[1] 90

# plot error rates vs the number of trees
plot(nt, mis, xlab = "Number of trees", ylab = "Error rate", type = "l", ylim = c(0.04,0.12),col="red")
lines(nt, mis1, xlab = "Number of trees", ylab = "Error rate", col="blue")
legend("topright",col=c("red","blue"),pch=1,legend=c("Adaboost","Random forests"))




############# EXAMPLES

fitb = blackboost(as.factor(Spam)~., data=train1, family=AdaExp(), 
                 control=boost_control(mstop=10))
# way 1: type="class"
pred = predict(fitb, test1, type = "class")  

# way 2: probility % 
# pred = predict(fitb, test1) 
# pred = as.integer(pred[,1]>0) # positive & negative

t = table(true=test1$Spam, pred=pred)
mis = 1-sum(diag(t))/sum(t)


fit <- randomForest(as.factor(Spam)~., data=train1, ntree=10)
pred <- predict(fit, test1)   # 0,1,...
t1 = table(true=test1$Spam, pred=pred)
mis1 = 1-sum(diag(t1))/sum(t1)
