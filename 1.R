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


