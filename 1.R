###### Assignment 1 Implementation of basis expansions

# read.csv(file, header = TRUE, sep = ",", dec=".")
# read.csv2(file, header = TRUE, sep = ";", dec=",")
data = read.csv2("C:/Users/Sam/Desktop/machine learning/lab1 block2/cube.csv")

### 1.1 Basis expansion function
myspline = function(x, y, knots){
  
  m=length(knots)
  n=length(x)
  H=matrix(0,nrow=n,ncol=m+2) #new features
  H[,1]=1
  H[,2]=x
  
  # loop in knots and ncol
  for(i in 1:m){
    # loop in nrow
    for(j in 1:n){
      #index_logical = ((knots[i]<=x&x<knots[i+1]))
      H[j,(i+2)] <- x[j] - knots[i]
      # from the (i+2)th col to last col, negative elements should be 0  
      if(H[j, (i+2)] < 0) H[j, (i+2)] = 0
    }
  }
  H
  
  # use matrix for linear regression fun()
  testx = H
  trainx = H
  testy = y
  trainy = y
  
  # Estimate w for linear regression
  w_hat = solve(t(trainx) %*% trainx) %*% t(trainx) %*% trainy
  
  # Prediction
  y_hat = testx %*% w_hat
  
  # plot predicted and the original data
  plot(x,y, col="blue")
  points(x,y_hat, col = "red", lwd=2)
  
}

### 1.2 Test with knots at 3 and 6
x = data$x
y = data$y
knots = c(2,4)
myspline(x, y, knots)

### 1.3 Use smooth.spline() to fit the same data
res1=smooth.spline(data$x,data$y,df=10)

# y_hat
pred = predict(res1,x=data$x)$y

# plot predicted and the original data
plot(x,y, col="blue")
lines(res1, col = "green", lwd=2)

