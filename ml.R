#####################################
# Estimate MSE of the optimal model by using the test set. 
pcr.fit1 = pcr(Viscosity~., ncomp=40,  data = train, scale = FALSE, validation = "none")  # 40######## 30
summary(pcr.fit1)

y1 = predict(pcr.fit1, newdata = test)
mse = mean((y1-test$Viscosity)^2,na.rm= TRUE)
mse


### 2.5 Divide the data into training(50%) and test(50%) sets.
n=dim(data)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.5)) 
train=data[id,] 
test=data[-id,]

#### 2.6 use PLS model
library(pls)
set.seed(12345)

# Estimate MSE of the optimal model by using the test set.
pls.fit = plsr(train$Viscosity~., data = train, scale = FALSE, validation = "CV")
summary(pls.fit)

# Plot MSPE
validationplot(pls.fit, val.type = "MSEP")

# Estimate MSE of the optimal model by using the test set. 
pls.fit2 = plsr(Viscosity~., ncomp=9,  data = train, scale = FALSE, validation = "none")
summary(pls.fit2)
y2 = predict(pls.fit2, newdata = test)
mse2 = mean((y2-test$Viscosity)^2,na.rm= TRUE)
mse2




