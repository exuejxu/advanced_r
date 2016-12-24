####### Assignment 2 regression tree analysis
data <- read.csv2("C:/Users/Sam/Desktop/machine learning/lab2 block2/bodyfatregression.csv")

### 2.1. compute the average error of a set of individual regression trees.
# hold-out test data
# use 2/3 of the data for training and 1/3 as hold-out test data.
n=dim(data)[1]
set.seed(1234567890) 
id=sample(1:n, floor(n*2/3)) 
train=data[id,] 
test=data[-id,] 

### bagging regression tree.iterations=100
library(tree)
mse1 = c()
for(B in 1:100){
  train_pos <- sample(1:nrow(train), size=nrow(train), replace = T)
  fittree = tree(Bodyfat_percent~., data = train[train_pos,])
  
  test_pos <- sample(1:nrow(test), size=nrow(test), replace = T)
  
  pred = predict(fittree,newdata=test[test_pos,])
  mse = mean((pred-test[test_pos,]$Bodyfat_percent)^2)
  mse1 = c(mse1, mse)
} 

mse1 #length 100
hist(mse1)
# upper bound of the squared error
# the bagged error is never larger than the average individual errors
mean(mse1)


### 2.2. 3-fold cross-validation
### bagging regression tree.iterations=100
n = dim(data)[1]
Nfolds = 3
k=floor(n/Nfolds)

index_folds <- c()
for (i in 1:k-1){
  index_folds[((i-1)*Nfolds+1):(i*Nfolds)] = sample(c(1:Nfolds), Nfolds)
}

index_folds[((k-1)*Nfolds+1):n] = sample(c(1:Nfolds), n-(k-1)*Nfolds, replace = TRUE)
index_folds 


mse2 = c()
for(j in 1:Nfolds){  
  # TRUE OR FALSE
  if_subset = (index_folds==j)
  test_cv = data[if_subset, ]
  train_cv = data[!if_subset, ]
  for (B in 1:100){
    train_pos <- sample(1:nrow(train_cv), size=nrow(train_cv), replace = T)
    fittree = tree(Bodyfat_percent~., data = train_cv[train_pos,])
    pred = predict(fittree,newdata=test_cv)
    mse = mean((pred-test_cv$Bodyfat_percent)^2)
    mse2 = c(mse2, mse)
  }
}

mse2 #length 300
hist(mse2)
# upper bound of the squared error
mean(mse2)


##################### built-in cv.res
mse2 = c()
for(i in 1:100){
  data_pos <- sample(1:nrow(data), size=nrow(data),replace = T)
  fit = tree(Bodyfat_percent~., data = data[data_pos,])
  cv.res = cv.tree(fit, K=3)
  
  # select the best number of leaves
  index <- which.min(cv.res$dev)
  bestsize <- cv.res$size[index]
  
  finalTree = prune.tree(fit, best = bestsize)
  
  pred = predict(finalTree, newdata=data)
  mse = mean((pred-data$Bodyfat_percent)^2)
  mse2 = c(mse2, mse)
} 

mse2

# upper bound of the squared error
mean(mse2) 
###################################################
