mis2G

### 2.3 Selecting optimal tree by train/validation
fitDe = tree(good_bad~., data = train, split = "deviance")

# rep(0,x) x decided by me, usually 9
trainScore = rep(0,15)
valiScore = rep(0,15)
for (i in 2:15){
  prunedTree = prune.tree(fitDe, best = i)
  pred = predict(prunedTree, newdata = valid, type = "tree")
  trainScore[i] = deviance(prunedTree)
  valiScore[i] = deviance(pred)
}

plot(2:15, trainScore[2:15], type = "b", col = "red", ylim = c(0, 600))
points(2:15, valiScore[2:15], type = "b", col = "blue")

# optimal no. of leaves
# as it is from 2 so, 3+1=4
which.min(valiScore[2:15]) + 1
# 4


# report its depth and the variables used by the tree.
# best = optimal no. of leaves
finalTree = prune.tree(fitDe, best = 4)
finalTree
summary(finalTree)

plot(finalTree)
text (finalTree , pretty = 0)

# Estimate the misclassification rate for the test data.
yhat = predict(finalTree, newdata = test, type = "class")
tab = table(ture=test$good_bad, pred=yhat)
mis = 1-sum(diag(tab))/sum(tab)
tab
mis

