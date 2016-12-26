mis

### 2.4 Classification using Naive Bayes
library(MASS)
library(e1071)

fitNB = naiveBayes(good_bad~., train)

# training data
yhatNB = predict(fitNB, newdata = train)
t1 = table(true=train$good_bad, pred=yhatNB)
misNB1 = 1-sum(diag(t1))/sum(t1)
t1
misNB1

# test data
yhatNBt = predict(fitNB, newdata = test)
t2 = table(true=test$good_bad,pred=yhatNBt)
misNB2 = 1-sum(diag(t2))/sum(t2)
t2
#      pred
#true   bad good
#bad   46   30
#good  49  125
misNB2

### 2.5 Repeat Naive Bayes by using loss matrix
# position of good/bad reversed!!!

# training data
library(MASS)
library(e1071)

fitNB = naiveBayes(good_bad~., train)

prob1 = predict(fitNB, newdata = train) #get class
prob1

classLoss <- function(fit, train){
  p <- predict(fit, newdata=train, type="raw") #get probabilty value
  #              bad         good
  #[1,] 7.317159e-01 0.2682840747
  #[2,] 4.136823e-02 0.9586317716
  return(factor(p[,2]/p[,1] > (10-0)/(1-0), labels=c("bad", "good")))
}
Yfit.train <- classLoss(fitNB, train)
Yfit.test <- classLoss(fitNB, test)
t3.train <- table(true=train$good_bad, pred=Yfit2.train)
t3.test <- table(true=test$good_bad, pred=Yfit2.test)
t3.train
t3.test

mis.train <- mean(Yfit2.train != train$good_bad)
mis.test <- mean(Yfit2.test != test$good_bad)
mis.train
mis.test
