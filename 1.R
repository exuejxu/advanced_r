### 1.2 implement LDA with proportional priors, (use only basic R functions)
### 1.3 plot classsification with a decision boundary
# Plot classifed data
plot(data$RW[label], data$CL[label], col="blue", 
     xlab = "Rear width", ylab = "Carapace length", 
     main = "Classified data by LDA")
points(data$RW[!label], data$CL[!label], col="red")

# plot decision boundary
range(data$CL)
# [1] 14.7 47.6
y = c(14:48)
# wi[1] for CL,y, wi[2] for RW,x
x = (w0i1-w0i2-(wi2[1]-wi1[1])*y)/(wi2[2]-wi1[2])

lines(x, y, lwd = 2)

### 1.4 plot classsification by logistic regression
# use function glm(), ##family=binomial##

sex = c()
class = glm(sex~CL+RW, family = "binomial", data = data)
p = predict(class, type = "response")
indexl = (p>=0.5)
# p[indexm] = 1, p[!indexm] = 0
# p = as.numeric(indexm)


# Plot classified data using logistic regression
# Plot classifed data
plot(data$RW[indexl], data$CL[indexl], col="blue", 
     xlab = "Rear width", ylab = "Carapace length", 
     main = "Classified by logistic regression")
points(data$RW[!indexl], data$CL[!indexl], col="red")

# plot decision boundary
range(data$RW)
#[1]  6.5 20.2
x1 = c(6:21) #xlab: RW

w = class$coefficients
#(Intercept)          CL          RW 
#13.616628      4.630747  -12.563893 
y1 = (-w[1]-w[3]*x1)/w[2]

lines(x1, y1, lwd = 2) #ylab: CL
