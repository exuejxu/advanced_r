##### Assignment 2. Analysis of credit scoring
df <- read.csv("C:/Users/Sam/Desktop/machine learning/lab3/creditscoring.csv")

### 2.1 Import the data to R and divide into training/validation/test as 50/25/25
n=dim(df)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.5)) 
train=df[id,] 

id1=setdiff(1:n, id)
set.seed(12345) 
id2=sample(id1, floor(n*0.25)) 
valid=df[id2,]

id3=setdiff(id1,id2)
test=df[id3,] 

### 2.2 Fit a decision tree to the training data
library(tree)

# Fit a decision tree using "Deviance" split =
fitDe = tree(good_bad~., data = train, split = "deviance")

# misclassification rates for the training and test data
yhat1D = predict(fitDe, newdata = train, type = "class")
tab1D = table(true=train$good_bad, pred=yhat1D)
mis1D = 1-sum(diag(tab1D))/sum(tab1D)
tab1D
mis1D 
Yfit2D = predict(fitDe, newdata = test, type = "class")
tab2D = table(true=test$good_bad, pred=Yfit2D)
mis2D = 1-sum(diag(tab2D))/sum(tab2D)
tab2D
mis2D 

# Fit a decision tree using "Gini" split = 
fitGi = tree(good_bad~., data = train, split = "gini")

yhat1G = predict(fitGi, newdata = train, type = "class")
tab1G = table(true=train$good_bad, pred=yhat1G)
mis1G = 1-sum(diag(tab1G))/sum(tab1G)
tab1G
mis1G
yhat2G = predict(fitGi, newdata = test, type = "class")
tab2G = table(true=test$good_bad, pred=yhat2G)
mis2G = 1-sum(diag(tab2G))/sum(tab2G)
tab2G
mis2G
