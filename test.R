#### Assignment 1. Spam classification with nearest neighbors

#### 1.1 Import data
data <- read.csv("C:/Users/Sam/Desktop/machine learning/lab1/spambase.csv")

# divide it into training and test sets (50%/50%)
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

traindata = as.matrix(train)
testdata = as.matrix(test)

#### 1.2 create K-nearest neighbors fun()
knearest <- function(data, k, newdata){
  X = data[,-ncol(data)]   #except the last col of spam
  Y = newdata[,-ncol(data)]
  
  # compute d(x,y) 
  Xhat = X/sqrt(rowSums(X^2))
  Yhat = Y/sqrt(rowSums(Y^2))
  
  # Compute distance 
  C = Xhat %*% t(Yhat)
  dist = 1-C 
  #View(dist) # 1370*1370
  
  # order index of the col of dist, 2 for col, increase, each col for each Y 
  order_dist = apply(dist,2,order) 
  #View(order_dist) # 1370*1370
  
  # Compute predicted class probabilities
  index_x = order_dist[1:k, ]
  #View(index_x) # 5*1370
  
  nearest = matrix(data[index_x,ncol(data)],nrow = k,ncol=ncol(order_dist)) 
  #View(nearest) # 5*1370
  ki = colSums(nearest)
  print(ki)
  prob = ki/k
  return(prob)
}

#knearest(traindata, k=5, testdata)
