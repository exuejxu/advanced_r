#CL5_ASS2
mydata = read.csv2("olive.csv", sep = ",", stringsAsFactors = FALSE)

# Construct variable R2
R2 = numeric(dim(mydata)[1])
for (i in 1: length(R2)){
  if(mydata$Region[i]==2){
    R2[i] = 1
  }else{
    R2[i] = 0
  }
}

# Plot Oleic versus Linoleic and color the observations by the value in R2
ind = (R2==1)
plot(mydata$linoleic[ind], mydata$oleic[ind], col = "blue", xlab = "Linoleic", ylab = "Oleic", xlim = c(500,1500), ylim = c(6000,8500))
points(mydata$linoleic[!ind], mydata$oleic[!ind], col = "red")

# Fit tie SVM models in which R2 is response and Oleic and Linoleic are predictors 
data1 = data.frame(Region = R2, Oleic = mydata$oleic, Linoleic = mydata$linoleic)
library(kernlab)

# Linear kernal
set.seed(12345)
fit1 = ksvm(Region~., data = data1, type = "C-svc",kernel="vanilladot")
Y1 = predict(fit1)
tab1 = table(data1$Region, Y1)
plot(fit1, data = data1)
mis1 = 1-sum(diag(tab1))/sum(tab1)

# RBF kernel
set.seed(12345)
fit2 = ksvm(Region~., data = data1, type = "C-svc",kernel="rbfdot")
Y2 = predict(fit2)
tab2 = table(data1$Region, Y2)
plot(fit2, data = data1)
mis2 = 1-sum(diag(tab2))/sum(tab2)

# RBF kernel with C=100
set.seed(12345)
fit3 = ksvm(Region~., data = data1, type = "C-svc",kernel="rbfdot", C = 100)
Y3 = predict(fit3)
tab3 = table(data1$Region, Y3)
plot(fit3, data = data1)
mis3 = 1-sum(diag(tab3))/sum(tab3)

# RBF kernel sigma = 10
set.seed(12345)
fit4 = ksvm(Region~., data = data1, type = "C-svc",kernel="rbfdot", kpar = list(sigma=10))
Y4 = predict(fit4)
tab4 = table(data1$Region, Y4)
plot(fit4, data = data1)
mis4 = 1-sum(diag(tab4))/sum(tab4)


# SVM with spoc-svc and linear kernel
data2 = data.frame(Region = mydata$Region, Palmitic = mydata$palmitic, Palmitoleic = mydata$palmitoleic, Stearic = mydata$stearic, Oleic = mydata$oleic, Linoleic = mydata$linoleic, Linolenic = mydata$linolenic, Arachidic = mydata$arachidic, Eicosenoic = mydata$eicosenoic)
fit5 = ksvm(Region~., data = data2, type = "spoc-svc", kernel="vanilladot", cross = 10)
Y5 = predict(fit5)
tab5 = table(data2$Region, Y5)
mis5 = 1-sum(diag(tab5))/sum(tab5)
