### 6.LASSO model
# use package glmnet with alpha=1 (LASSO)
# Channel1-Channel100
covariates <- scale(data[, 2:101])
# Fat
response <- scale(data[, 102])
model2=glmnet(as.matrix(covariates), response, alpha=1,family="gaussian")
plot(model2, xvar="lambda", label=TRUE)

### 7.Use cross-validation to find the optimal LASSO model
# if ridge model alpha=0
# Channel1-Channel100
covariates <- scale(data[, 2:101])
# Fat
response <- scale(data[, 102])
model3=cv.glmnet(as.matrix(covariates), response, alpha=1,family="gaussian")
model3$lambda.min
plot(model3)
coef <- coef(model3, s="lambda.min")
coef
#101 x 1 sparse Matrix of class "dgCMatrix"
#1
#(Intercept)  2.430320e-15
#Channel1     .           
#Channel2     .           
#Channel3     .   

# counte variables chosen by the model
sum = 0
for(i in 2:100){
  if (coef[i,1] != 0){
    sum = sum + 1
    sum
  }
}
print(sum)
#[1] 13
