library(MASS)

# Ridge regression by using n-fold cross-validation
RidgeRegre = function (X, Y, Lam, Nfolds){
  
  N = length(Y)
  K = ceiling(N/Nfolds)
  
  #X = scale(X, scale = FALSE)
  #Y = scale(Y, scale = FALSE)
  
  # make n subsets of data
  indxFolds = numeric(N)
  for (kk in 1:K-1){
    set.seed(12345)
    indxFolds[((kk-1)*Nfolds+1):(kk*Nfolds)] = sample(c(1:Nfolds))
  }
  set.seed(12345)
  indxFolds[((K-1)*Nfolds+1):N] = sample(c(1:Nfolds), N-(K-1)*Nfolds)
  #indxFolds = c(1,2,3,4,5,6,7,8,9,10,10,10,10,10,10,10)
  CV = 0;

  #model fit for each fold
  for (ii in 1:Nfolds){
    
    valInd = (indxFolds==ii)
    
    # divide data into training data and validation data
    validX = X[valInd,]
    trainX = X[! valInd,]
    validY = Y[valInd]
    trainY = Y[! valInd]
    
    trainX1 = scale(trainX, center = TRUE, scale = FALSE)
    trainY1 = scale(trainY, center = TRUE, scale = FALSE)
    if (is.null(dim(validX))){
      validX1 = validX-colMeans(trainX)
    }
    else
    {
      validX1 = scale((validX),(colMeans(trainX)), FALSE)
    }
    
    validY1 = validY-mean(trainY)
      
    # Estimate w
    I = diag(dim(trainX1)[2])
    estW = ((solve((t(trainX1) %*% trainX1) + Lam *I)) %*% t(trainX1) )%*%trainY1
    # Prediction
    estY = validX1 %*% estW
    
    #calculate sum of cross-validation score
    sqsum = sum((validY1-estY)^2)
    CV = CV+sqsum
    

  }  

  # Return CV score
  return(CV/N)
}

# load data
data = unname(as.matrix(longley))
dataX = data[,1:(dim(longley)[2]-1)]
dataY = matrix(longley$Employed)

# Lambda 
L = c(1:7)

# Number of folds
Nfolds = 10
CV = numeric(length(L))

for (jj in 1:length(L)){
  # Calculate cross validation value for each lambda
  CV[jj] =  RidgeRegre(as.matrix(dataX), dataY, L[jj], Nfolds)
}

plot(L,CV, col="red", xlab = "Lambda values", ylab = "CV scores") 
