####### Assignment 2. Principal components
data <- read.csv2("C:/Users/Sam/Desktop/machine learning/lab4/NIRSpectra.csv")

### 2.1 Conduct a standard PCA
# by using the feature space
# plot to explain how much variation is explained by each feature.

data1=data
data1$Viscosity=c()
res=prcomp(data1)
lambda=res$sdev^2
#eigenvalues
lambda
#proportion of variation
# Select the minimal number of components 
# explaining at least 99% of the total variance.
sprintf("%2.3f",lambda/sum(lambda)*100)
screeplot(res)

# Select the minimal number of components 
# explaining at least 99% of the total variance.
# scores plot in the coordinates (PC1, PC2)
plot(res$x[,1], res$x[,2])


### 2.2 Trace plots for the loadings of PC1 & PC2.

#U = loadings(res)
U=res$rotation
head(U)

plot(U[,1], main="Traceplot, PC1")
plot(U[,2],main="Traceplot, PC2")


### 2.3 ICA
# Perform Independent Component Analysis of PC1 & PC2, n.comp=2
set.seed(12345) 
library(fastICA)
a = fastICA(data1, n.comp=2,alg.typ = "parallel", fun = "logcosh", alpha = 1, 
            method = "R", row.norm = FALSE, maxit = 200, tol = 0.0001, verbose = TRUE)
# Report matrix W
a$W

# Explain the roll of the matrix w'= k.w
Wp = a$K%*%a$W
Wp

# present its columns in form of the trace plots.
plot(Wp[,1], main = "Traceplot, PC1")
plot(Wp[,2], main = "Traceplot, PC2")

# Make the score plot of the first two latent features
plot(a$S[,1], a$S[,2])


### 2.4 Fit a PCR model to the training data 
# where number of components is selected by cross validation.
library(pls)
set.seed(12345)

# where number of components is selected by cross validation.
pcr.fit = pcr(Viscosity~., data = data, scale = FALSE, validation = "CV")
summary(pcr.fit)

# Plot MSPE, the dependence of the mean-square predicted error
validationplot(pcr.fit, val.type = "MSEP")


