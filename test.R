#### Assignment 1. Spam classification with nearest neighbors

#### 1.3 Classify the training and test data by using K=5
# report the confusion matrix (use table())
# misclassification rate for training and test data

# test data
fit1 = knearest(traindata, k=5, testdata) # fit:probility %
predY1 = as.integer(fit1>0.5)   # pred: classify(0,1) as.integer:(TRUE FALSE)->(1,0) 
true1 = testdata[,ncol(data)]3
conf1 = table(true1,predY1)
misc1 = (1-sum(diag(conf1))/length(true1))
conf1
misc1

# training data
fit2 = knearest(traindata, k=5, traindata)
predY2 = as.integer(fit2>0.5)
true2 = traindata[,ncol(data)]
conf2 = table(true2,predY2)
misc2 = (1-sum(diag(conf2))/length(true2))
conf2
misc2
