

#### Question 1: Principal components ####
dat = read.table("C:/732A97 Multivariate Statistical Methods/lab/T1-9.dat", 
                 header=F)
### 1a
R = cor(dat[,2:8])
R
eigen(R)

### 1b
# standarized data
mu = colMeans(dat[,2:8])
sd = sapply(dat[,2:8], sd)

z = dat[,2:8]
for(j in 1:dim(dat[,2:8])[2]){
  z[,j] = (dat[,2:8][,j] - mu[j]) / sd[j]
}

R_z = cor(z)
comp1 = -eigen(R_z)$vectors[ ,1]
comp2 = eigen(R_z)$vectors[ ,2]
rbind(comp1,comp2)

# table of corr between z and first 2 components
y1 = as.matrix(z) %*% as.matrix(comp1)
y2 = as.matrix(z) %*% as.matrix(comp2)
rbind(cor(y1,z),cor(y2,z))

# cumulative percentage of total sample varance explained by 
# the first two components is
sum(eigen(R_z)$values[1:2]) / sum(eigen(R_z)$values)
# [1] 0.919474

### 1d
dat[order(y1),1]
# [1] USA GER RUS CHN FRA GBR CZE POL ROM AUS

#### Question 2: Factor analysis ####
S = cov(dat[,2:8])

# PC estimation
library(psych)

fit1 = principal(dat[,2:8], nfactors=2, rotate="none")
fit1$loadings

fit2 = principal(dat[,2:8],nfactors=2,rotate="varimax")
fit2$loadings

factor_score_eff1 = t(matrix(fit1$loadings,ncol=2)) %*% solve(R)
# factor score for all data
f = matrix(NA,ncol = nrow(z),nrow = 2)
for(i in 1:nrow(z)) 
  f[,i] = factor_score_eff1 %*% t(as.matrix(z[i,]))
f

# find outliers
plot(f[1,],f[2,],xlab="First Factor",ylab="Second Factor",
     main = "PC Estimation with Rotated Laodings",
     cex = 0.8)

# lable the outliers
index = order(f[1,],decreasing = T)[1:3]
# [1] 46 11 40
dat$V1[index]
# [1] SAM COK PNG

plot(f[1,-index],f[2,-index],xlab="First Factor",ylab="Second Factor",
     main = "PC Estimation with Rotated Laodings",
     cex = 0.8, col = "blue",xlim=c(-1.5,3.5),ylim=c(-3.2,3))
points(f[1,index],f[2,index],cex = 0.8, col = "red",type = "p" )

# ML estimation
fit3 <- factanal(dat[,2:8], 2, rotation="none")
fit3$loadings

fit4 <- factanal(dat[,2:8], 2, rotation="varimax")
fit4$loadings

factor_score_eff2 = t(matrix(fit4$loadings,ncol=2)) %*% solve(R)
# factor score for all data
f2 = matrix(NA,ncol = nrow(z),nrow = 2)
for(i in 1:nrow(z)) 
  f2[,i] = factor_score_eff2 %*% t(as.matrix(z[i,]))
f2

# find outliers
plot(f2[1,],f2[2,],xlab="First Factor",ylab="Second Factor",
     main = "Principal Component Factor Analysis",
     cex = 0.8)

# lable the outliers
ind_x = order(f2[1,],decreasing = T)[1]
# [1] 46 
ind_y = order(f2[2,],decreasing = T)[1:2]
# 11 31
index2 = c(ind_x,ind_y)
dat$V1[index2]
# [1] SAM  COK  KORN

plot(f2[1,-index2],f2[2,-index2],xlab="First Factor",ylab="Second Factor",
     main = "Maximum Likelihood Factor Analysis",
     cex = 0.8, col = "blue",xlim=c(-1.5,5.5),ylim=c(-3.2,3))
points(f2[1,index2],f2[2,index2],cex = 0.8, col = "red",type = "p" )
