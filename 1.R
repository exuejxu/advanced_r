data("swiss")
# Fertility: Y
# All other variables: X
# Nfolds: 5 number of folds
Y <- as.matrix(swiss[,1])
X <- as.matrix(swiss[,-1])
Nfolds <- 5

n = length(Y) #47

# permute the data
set.seed(12345)
ind=sample(n,n)
X=X[ind,]
Y=Y[ind]
      
k = floor(n/Nfolds) #9
p=ncol(X)

# permute 1:k-1 subsets
# (if n=51,Nfolds=10,k=5,first 8 subsets have 5 elements)
index_folds <- c()
# permute 1:k-1 subsets
for (i in 1:k-1){
  # 1:5, 6:10, 11:15,...,36:40
  index_folds[((i-1)*Nfolds+1):(i*Nfolds)] = sample(c(1:Nfolds), Nfolds)
}

# permute k_th/last subset
# last 41:47 sample(1:10, 11, replace = T)
index_folds[((k-1)*Nfolds+1):n] = sample(c(1:Nfolds), n-(k-1)*Nfolds, replace = TRUE)
index_folds 
#47 index
# [1] 2 1 3 4 5 1 2 5 4 3 5 4 3 1 2 1 3 4 2 5 2 5 1 4 3 4 3 1 2 5 1 5 4 3 2 3 4 1 2
#[40] 5 1 4 4 3 4 4 1
#model fit by using each fold as test
MSE=numeric(2^p-1)
Nfeat=numeric(2^p-1)
Features=list()
curr = 0
for (f1 in 0:1)
  for (f2 in 0:1)
    for(f3 in 0:1)
      for(f4 in 0:1)
        for(f5 in 0:1){
          model= c(f1,f2,f3,f4,f5)
          if (sum(model)==0) next()
          SSE=0
          
          for (j in 1:Nfolds){
            # T OR F for row
            if_subset = (index_folds==j)
            #> valInd 51 elements if j=1
            #[27] FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
            #[40] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
            
            # T OR F for col
            model2 = (model == 1)
            
            # assume matrix X has 5 columns
            testx = X[if_subset, model2]
            trainx = X[!if_subset, model2]
            testy = Y[if_subset]
            trainy = Y[!if_subset]
            
            # Estimate w for linear regression
            w_hat = (solve(t(trainx) %*% trainx) %*% t(trainx)) %*% trainy
            
            # Prediction
            y_hat = testx %*% w_hat
            SSE = SSE + sum((testy-y_hat)^2)
            
          }
          curr=curr+1
          MSE[curr]=SSE/n
          Nfeat[curr]=sum(model)
          Features[[curr]]=model
        }
plot(Nfeat, MSE, col="red", xlab = "No. of features", 
     ylab = "CV scores")

m=which.min(MSE)
list(MSE=MSE[m], Features=Features[[m]])
