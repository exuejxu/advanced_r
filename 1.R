# read data file
mydata = read.csv2("C:/Users/Sam/Desktop/machine learning/zyx ml lab/spambaseshort.csv", sep = ";", dec = ",") 
# Split data into training and test sets
set.seed(12345)
ind = sample(n, floor(n*0.7))
train = mydata[ind,]
test = mydata[-ind,]
# Logistic regression
dataLR = mydata
# Change the class of spam -1 to 0
dataLR$Spam[dataLR$Spam==-1] = 0
trainLR = dataLR[ind,]
testLR = dataLR[-ind,]
# Fit a logistic regression model on the training data
fit = glm(Spam~., data = trainLR, family = "binomial")
# Predict the class of the test set
pre = predict(fit, testLR, type = "response")
indm = pre>=0.5
y = numeric(nrow(testLR))
y[indm] = 1
y[!indm] = 0

tabLR = table(y, testLR$Spam)
misLR = 1-sum(diag(tabLR))/sum(tabLR)
