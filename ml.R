###### Assignment 3
mydata = read.csv("C:/Users/Sam/Desktop/machine learning/old exam/wine.csv")
View(mydata)

### 3.1 change the class "2" to "-1"
# in order to model a classification problem using the neuralnet package.
for(i in 1:nrow(mydata)){
  if(mydata[i, 1] == 2) mydata[i, 1] = -1
}
View(mydata)

### 3.2 Separate the data set into 70 percent training/ 30 percent test
set.seed(12345) 
n=dim(mydata)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.7)) 
train2=mydata[id,] 
test2=mydata[-id,] 

### 3.3 Fit a neural network
library(neuralnet)
set.seed(12345)
# 0 hidden nodes and tanh as the activation function
model1 <- neuralnet(class~alcohol+malic_acid+ash+ash_alcalinity+magnesium+total_phenols+flavanoids+nonflavanoid_phenols+proanthocyanins+color_intensity+hue+OD280+proline, data = train2, hidden = 0, act.fct = "tanh")
model1
# Estimated Network Weights 
model1$result.matrix
# Plot model
plot(model1)

### 3.4 Compute the misclassification rate of the training and test set
# plot fitted values
# train2[,-1]: all cols of features(expect the object:class) 
pred1 = compute(model1, train2[,-1])$net.result
# convert 0,1 to -1,1
pred1 = as.integer(pred1>0)*2-1
true1 = train2$class
conf1 = table(true1,pred1)
misc1 = (1-sum(diag(conf1))/length(true1))
conf1
misc1

# test set
pred2 = compute(model1, test2[,-1])$net.result
# convert 0,1 to -1,1
pred2 = as.integer(pred2>0)*2-1
true2 = test2$class
conf2 = table(true2,pred2)
misc2 = (1-sum(diag(conf2))/length(true2))
conf2
misc2
