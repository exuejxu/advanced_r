# 0.1



### 1.2. Compute the test error and the number of the contributing features
### a.	Elastic net
# with the binomial response: family
# penalty is selected by the cross-validation #####

library(glmnet)
# For either "binomial" or "multinomial", y will be coerced into a factor.
# train data
# predictors
x2=as.matrix(train[,-4703])
# target
y2=train[[4703]]

# test data
# predictors
x3=as.matrix(test[,-4703])
# target
y3=test[[4703]]

cv.fit = cv.glmnet(x=x2, y=as.factor(y2), family="binomial", alpha = 0.5) ### alpha ###
pred = predict.cv.glmnet(cv.fit, newx=x3, type = "class") #class: get 0 and 1
pred = as.numeric(pred)

tab = table(true=y3, pred=pred)
tab
# compute misclassification rates/ test error
mis = 1-sum(diag(tab))/sum(tab)
mis
#[1] 0.2


# number of contributing features ##########
features <- cv.fit$nzero[which(cv.fit$lambda == cv.fit$lambda.min)]
#s31 
#33 


### b.	Support vector machine with vanilladot kernel.
library(kernlab)

fit1 = ksvm(Conference~., data = train, type = "C-svc", kernel="vanilladot")  ### Warning but ok!
Y1 = predict(fit1, test)

tab1 = table(true=test$Conference, pred=Y1)
tab1
# compute misclassification rates
mis1 = 1-sum(diag(tab1))/sum(tab1)
mis1
#[1] 0.05
