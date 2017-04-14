##### Assignment 1. Feature selection by cross-validation in a linear model.
cv <- function(X, Y, Nfolds){
  n = length(Y) #47
  
  # permute the data
  set.seed(12345)
  ind=sample(n,n)
  X=X[ind,]
  Y=Y[ind]
  
  k = floor(n/Nfolds) #9
  p=ncol(X)
  
  index_folds <- c()
  # permute 1:k-1 subsets
  for (i in 1:k-1){
    set.seed(12345)
    index_folds[((i-1)*Nfolds+1):(i*Nfolds)] = sample(c(1:Nfolds), Nfolds)
  }
  
  # permute k_th/last subset
  set.seed(12345)
  index_folds[((k-1)*Nfolds+1):n] = sample(c(1:Nfolds), n-(k-1)*Nfolds, replace = TRUE)
  index_folds 
  
  MSE=numeric(2^p-1)
  Nfeat=numeric(2^p-1)
  Features=list()
  curr = 0
  # select features (2^p-1) combinations
  for (f1 in 0:1)
    for (f2 in 0:1)
      for(f3 in 0:1)
        for(f4 in 0:1)
          for(f5 in 0:1){
            model= c(f1,f2,f3,f4,f5)
            if (sum(model)==0) next()
            model = c(f1*1,f2*2,f3*3,f4*4,f5*5)
            
            SSE=0
            for (j in 1:Nfolds){
              # T OR F for row
              if_subset = (index_folds==j)
              
              # add bias!! X=cbind(1,X)
              testx = cbind(1, X[if_subset, model])
              trainx = cbind(1, X[!if_subset, model])
              testy = Y[if_subset]
              trainy = Y[!if_subset]
              
              # Estimate w for linear regression
              w_hat = solve(t(trainx) %*% trainx) %*% t(trainx) %*% trainy
              
              # Prediction
              y_hat = testx %*% w_hat
              SSE = SSE + sum((testy-y_hat)^2)
              
            }
            curr=curr+1
            SSE = SSE/Nfolds
            MSE[curr]=SSE/n 
            Nfeat[curr]=sum(model)
            Features[[curr]]=model[model!=0]
           
          }
  
  #print(Features) #ttl 31
  plot(1:length(MSE), MSE, col="red", xlab = "No. of features", 
       ylab = "CV scores")
  print(MSE)
  m=which.min(MSE)
  list(MSE=MSE[m], Features=Features[[m]])
}

### 2. Test your function on data set swiss
data("swiss")

Y <- as.matrix(swiss[,1])
X <- as.matrix(swiss[,-1])
Nfolds <- 5
cv(X=X, Y=Y, Nfolds = Nfolds)
