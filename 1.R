#### 1.5 Use classifier kknn() fr package kknn  with K=5
# report the confusion matrix & misclassification rate for test data

library(kknn)

# test data
# Way 1: can compared with different value, not only 0.5
fit5 = kknn(as.factor(train$Spam)~., train, test, k = 5)
prob5 = fit5$prob # 2 cols: 1st col for 0,2nd col for 1
predY5 = as.integer(prob5[,2]>0.5)
true5 = testdata[, ncol(data)]
conf5 = table(true5,predY5)
misc5 = (1-sum(diag(conf5))/length(true5))
conf5
misc5

# way 2: if compared with 0.5
fit52 = kknn(as.factor(train$Spam)~., train, test, k = 5)
predY52 <- fitted(fit52)
true52 = testdata[, ncol(data)]
conf52 = table(true52, predY52)
conf52

# package(class) knn() 
library(class)
fit6 = knn(train, test, as.factor(train$Spam), k=5,prob = TRUE)
prob6 = attributes(fit6)$prob
prob6 = ifelse(fit6=="1", prob6, 1-prob6)
predY6 = as.integer(prob6>0.5)
true6 = testdata[, ncol(data)]
conf6 = table(true6,predY6)
misc6 = (1-sum(diag(conf6))/length(true6))
conf6
misc6
