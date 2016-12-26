###### Assignment 1. High-dimensional methods
data0 = read.csv2("C:/Users/Sam/Desktop/machine learning/lab3 block2/data.csv")


### 1.1. Divide data into training and test sets (70/30) without scaling.
n=dim(data0)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.7)) 
train=data0[id,]
test=data0[-id,]

# Perform nearest shrunken centroid classification of training data
data=as.data.frame(train) #with scaling: data=as.data.frame(scale(data)) 
data$Conference=as.factor(train$Conference) 

library(pamr)
rownames(data)=1:nrow(data)
# predictors
x=as.matrix(t(data[,-4703])) 
# target
y=data[[4703]] #data$Conference

mydata=list(x=x,y=as.factor(y),geneid=as.character(1:nrow(x)), genenames=rownames(x))
model=pamr.train(mydata,threshold=seq(0,4, 0.1))

# threshold is chosen by cross-validation.
cvmodel=pamr.cv(model,mydata)
# find value of threshold with min error 1.4
print(cvmodel) 
# centroid plot
pamr.plotcv(cvmodel) 

# centroid plot by the optimal threshold
pamr.plotcen(model, mydata, threshold=1.3)  
pamr.plotcen(model, mydata, threshold=1.4) 

# get features' name list
a=pamr.listgenes(model,mydata,threshold=1.4) 
cat(paste(colnames(data)[as.numeric(a[,1])], collapse='\n' ) )

# test error for test dataset
test$Conference=as.factor(test$Conference) 
rownames(test)=1:nrow(test)
# predictors
xt=as.matrix(t(test[,-4703])) 
# target
yt=test[[4703]] #data$Conference

pred_test <- pamr.predict(model, newx = xt, threshold = 1.4, type = "class")
confusion_test <- table("true" = yt, "predicted" = pred_test)
test_error <- 1 - sum(diag(confusion_test))/sum(confusion_test)
test_error
# 0.1
