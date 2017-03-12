
source("C:/Users/Sam/Desktop/Computational Statistics/lab4/Code_Lec4_sl18.R")
vmean<-c(1,2)
mVar<-rbind(c(1,0.5),c(0.5,1))
nstep<-200
X0<-c(10,10)
mX<-f.MCMC.Gibbs(nstep,X0,vmean,mVar)

plot(mX[-1,1],mX[-1,2],pch=19,cex=0.5,col="black",xlab="X1",ylab="X2",main="",cex.lab=1.7,cex.axis=1.5,xlim=c(min(mX[-1,1]-0.5),max(mX[-1,1]+0.5)),ylim=c(min(mX[,2]-0.5),max(mX[-1,2]+0.5)))

par(mfrow=c(2,1))
plot(2:nstep,mX[-1,1],pch=19,cex=0.3,col="black",xlab="t",ylab="X1",cex.axis=1.5,cex.lab=1.7)
abline(h=vmean[1])
plot(2:nstep,mX[-1,2],pch=19,cex=0.3,col="black",xlab="t",ylab="X2",cex.axis=1.5,cex.lab=1.7)
abline(h=vmean[2])

library(coda)
f1<-mcmc.list()
f2<-mcmc.list()
n<-100
k<-20
X1<-matrix(rnorm(n*k),ncol=k,nrow=n)
X2<-X1+(apply(X1,2,cumsum)*(matrix(rep(1:n,k),ncol=k)^2))
for (i in 1:k){
  f1[[i]]<-as.mcmc(X1[,i])
  f2[[i]]<-as.mcmc(X2[,i])
}
print(gelman.diag(f1))
# Potential scale reduction factors:
#     Point est. Upper C.I.
#[1,]      0.999       1.01

print(gelman.diag(f2))
# Potential scale reduction factors:
#     Point est. Upper C.I.
#[1,]       1.82       2.38

f.OU.MC<-function(nstep,X0){
    vN<-1:nstep
    vX<-rep(X0,nstep)
    X<-X0
    for (i in 2:nstep){
      X<-exp(-0.1)*X+rnorm(1,0,sqrt(2.5*(1-exp(-0.2))))
      vX[i]<-X
    }
    plot(vN,vX,pch=19,cex=0.3,col="black",xlab="t",ylab="X(t)",main="",ylim=c(min(X0-0.5,-5),max(5,X0+0.5)))
    abline(h=0)
    abline(h=1.96)
    abline(h=-1.96)
}
f.MCMC.MH<-function(nstep,X0,props){
    vN<-1:nstep
    vX<-rep(X0,nstep)
    for (i in 2:nstep){
      X<-vX[i-1]
      Y<-rnorm(1,mean=X,sd=props)
      u<-runif(1)
      a<-min(c(1,(dnorm(Y)*dnorm(X,mean=Y,sd=props))/(dnorm(X)*dnorm(Y,mean=X,sd=props))))
      print(a)
      if(u <= a) vX[i]<-Y
      else vX[i]<-X 
    }
    plot(vN,vX,pch=19,cex=0.3,col="black",xlab="t",ylab="X(t)",main="",ylim=c(min(X0-0.5,-5),max(5,X0+0.5)))
    abline(h=0)
    abline(h=1.96)
    abline(h=-1.96)
}

# q normal with sd:props
f.MCMC.MH(500,-10,0.5)

f.MCMC.MH(500,-10,0.1)

f.MCMC.MH(500,-10,20)
f.MCMC.Gibbs<-function(nstep,X0,vmean,mVar){
    vN<-1:nstep
    d<-length(vmean)
    mX<-matrix(0,nrow=nstep,ncol=d)
    mX[1,]<-X0 # starting point
    
    for(i in 2:nstep){
      X<-mX[i-1,] # each sample point
      Y<-rep(0,d) # each dimension
      # conditional dist
      Y[1]<-rnorm(1,mean=vmean[1]+mVar[1,1]%/%mVar[-1,-1]%*%mVar[1,-1]%*%(X[2:d]-vmean[-1]),sd=sqrt((mVar[1,1]-mVar[1,-1]%*%mVar[-1,1])%*%(mVar[-1,-1])))
      for(j in 2:(d-1)){
        Y[j]<-rnorm(1,mean=vmean[j]+(mVar[j,j]%*%mVar[-j,-j]%*%mVar[j,-j]%*%(c(Y[1:(j-1)],X[(j+1):d])-vmean[-j])),sd=sqrt(mVar[j,j]-mVar[j,-j]%*%solve(mVar[-j,-j])%*%mVar[-j,j]))
      }
      Y[d]<-rnorm(1,mean=vmean[d]+(mVar[d,d]%*%mVar[-d,-d]%*%mVar[d,-d]%*%(Y[1:(d-1)]-vmean[-d])),sd=sqrt(mVar[d,d]-mVar[d,-d]%*%solve(mVar[-d,-d])%*%mVar[-d,d]))
      mX[i,]<-Y
    }
    mX
}

vmean<-c(1,2)
mVar<-rbind(c(1,0.5),c(0.5,1))
nstep<-200
X0<-c(10,10)
mX<-f.MCMC.Gibbs(nstep,X0,vmean,mVar)
