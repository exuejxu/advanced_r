###### Assignment 1
data = read.csv2("C:/Users/Sam/Desktop/machine learning/old exam/glass.csv")
View(data)

### 1.1 Partition data into training, validation and test sets (50/25/25) 
# by using seed 12345.
n=dim(data)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.5)) 
train=data[id,] 

id1=setdiff(1:n, id)
set.seed(12345) 
id2=sample(id1, floor(n*0.25)) 
valid=data[id2,]

id3=setdiff(id1,id2)
test=data[id3,] 

# Al as target variable & remaining variables as features in the models
# Use training and validation trees to fit the regression trees of different sizes 
# and estimate the predictive error.
library(tree)
fit = tree(Al~., data = train)

cv.res = cv.tree(fit) #cross-validation
plot(cv.res$size, cv.res$dev, type = "b", col = "red")
plot((cv.res$k), cv.res$dev, type = "b", col = "red")
plot(cv.res)
# biggest size is 8.


# rep(0,x) x decided by me, usually 9
trainScore = rep(0,8)
valiScore = rep(0,8)
for (i in 2:8){
  prunedTree = prune.tree(fit, best = i)
  pred = predict(prunedTree, newdata = valid, type = "tree")
  trainScore[i] = deviance(prunedTree)
  valiScore[i] = deviance(pred)
}

plot(2:8, trainScore[2:8], type = "b", col = "red", ylim = c(0, 20))
points(2:8, valiScore[2:8], type = "b", col = "blue")

# optimal no. of leaves
# as it is from 2 so, 3+1=4
which.min(valiScore[2:8]) + 1
# 5

#### 1.2 report its depth and the variables used by the tree.
# best = optimal no. of leaves
finalTree = prune.tree(fit, best = 5)
finalTree
summary(finalTree)

plot(finalTree)
text (finalTree , pretty = 0)

# report the test error
pred = predict(fit,newdata=test)
mse = mean((test$Al-pred)^2)
mse
# [1] 0.1120781

### 1.3 Fit a PLS regression model 
# where the amount of variables are chosen by cross validation

library(pls)
pls.fit = plsr(Al~., data = train, scale = FALSE, validation = "CV")
summary(pls.fit)

# Plot MSPE
validationplot(pls.fit, val.type = "MSEP")

# Estimate MSE of the optimal model by using the test set. minimum MSEP
#ncomp=6, minimum MSEP
pls.fit2 = plsr(Al~., ncomp=6,  data = test, scale = FALSE, validation = "none")
summary(pls.fit2)
y2 = predict(pls.fit2, newdata = test)
mse2 = mean((y2-test$Al)^2,na.rm= TRUE)
mse2
# [1] 0.02914101
