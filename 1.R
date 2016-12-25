### 1.2 implement LDA with proportional priors, (use only basic R functions)
# inputs RW and CL and output Sex

# class set for input male & female for RW & CL
index = (data$sex=="Male")

RW1 = data$RW[index]   #male
RW2 = data$RW[!index]  #female

CL1 = data$CL[index]   #male
CL2 = data$CL[!index]  #female

nc1 = length(CL1)  #length(CL1)=length(RW1)
nc2 = length(CL2)

k = 2

mu1 = c(mean(CL1),mean(RW1))
mu2 = c(mean(CL2),mean(RW2))

# compute sigma1 of male, size: 2*2 for cl & rw
sigma1 = matrix(0, nrow = 2, ncol = 2)   
for (i in 1:length(CL1)){
  xi = c(CL1[i], RW1[i])
  temp = (xi-mu1)%*%(t(xi-mu1))
  sigma1 = sigma1+temp
}
sigma1 = sigma1/nc1

# compute sigma2 of female, size: 2*2 for cl & rw
sigma2 = matrix(0, nrow = 2, ncol = 2)
for (i in 1:length(CL2)){
  xi = c(CL2[i], RW2[i])
  temp = (xi-mu2)%*%(t(xi-mu2))
  sigma2 = sigma2+temp
}
sigma2 = sigma2/nc2

sigma = (nc1*sigma1 + nc2*sigma2)/(nrow(data))

# compute coefficients for two classes
pihat1 = nc1/nrow(data) 

w0i1 = -0.5*t(mu1)%*%solve(sigma)%*%mu1+log(pihat1)
wi1 = solve(sigma)%*%mu1
w0i1
wi1

pihat2 = nc2/nrow(data)  

w0i2 = -0.5*t(mu2)%*%solve(sigma)%*%mu2+log(1-pihat2)
wi2 = solve(sigma)%*%mu2
w0i2
wi2

# Label data based on LDA 
label = c()
for (i in 1:nrow(data)){
  x = c(data$CL[i], data$RW[i])
  p1 = (w0i1 + t(wi1)%*%x)
  p2 = (w0i2 + t(wi2)%*%x)
  pmale = exp(p1)/(exp(p1)+exp(p2))
  label[i] = (pmale>=0.5)
}
label
