fthreebits<-function(k,s,L,N){
    X0<-4*s+1;a<-8*k+5;m<-2^L;X<-X0
    for (i in 1:N){
	print(c(X,rev(intToBits(X)[1:5])))
	X<-(a*X)%%m ##c=0
    }
}
fthreebits(2,3,8,10)

fCongGenGrid<-function(a,m,c,X0,N){
    X<-X0
    vU<-rep(X0,N)
    for (i in 1:N){vU[i]<-X;X<-(a*X+c)%%m}
    vU<-vU/m
    plot(vU[1:(N-1)],vU[2:N],pch=19,cex=0.8,main="",xlab="Xk/m",ylab="Xk/(m+1)")
}
fCongGenGrid(a=17,m=131,c=8,X0=4,N=200)

