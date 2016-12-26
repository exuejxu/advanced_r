###### Assignment 2. Online learning

set.seed(1234567890)
spam <- read.csv2("C:/Users/Sam/Desktop/machine learning/lab3 block2/spambase.csv")

ind<- sample(1:nrow(spam))
spam <- spam[ind,c(1:48,58)]

gaussian_k<- function(xi, sv, h=1){ # Gaussian kernel classifer
  dis = c()
  kl = c()
  for(m in sv){
    # use dist() to compute the Euclidean distance, attributes:48
    # dist in 48-dimensional space
    dist= dist(rbind(xi, spam[m, 1:48]), method="euclidean")
    dis = c(dis, dist)
    #print(dis)
  }
  for(i in 1:length(dis))
    kl[i] = exp(-(dis[i]/h)^2/2)
  return(kl) #m
} 



SVM <- function(sv,i, kl) { # SVM on point i with support vectors sv
  
  # labels in spambase.csv are 0/1 and SVMs need -1/+1. 
  # use 2*label-1 # to convert from 0/1 to -1/+1
  # train target
  y = spam[, 49]
  y = 2*y-1
  yp = 0
  for(i in 1:length(sv)){
    res = 1*y[sv[i]]*kl[i]
    yp = yp + res
  }
  return(yp)
}



online_learning <- function(M, beta){
  N <- 500
  errorrate<- vector(length = N)
  errorrate[1] <- 1
  sv <- c(1)
  yp = c(1)
  
  for(i in 2:N){
    
    # call gaussian kernel function
    kl = gaussian_k(xi=spam[i, 1:48], sv = sv)
    #print(kl)
    
    # call SVM function
    # all yp for i
    yp[i] = SVM(sv=sv, i=i, kl = kl)
    #print(yp[i])
    
    # error rate
    y = spam[, 49]
    # convert yp to yhat class c(1,0)
    yhat = as.integer(yp >= 0) 
    tab = table(true=y[2:i], pred=yhat[2:i])
    # compute misclassification rates/ error rate
    errorrate[i] = 1-sum(diag(tab))/sum(tab)
    
    y = 2*y-1
    #print(y[i]*yp[i])
    if((y[i]*yp[i]) <= beta){     #beta
      sv = c(sv, i)
      sv
      #print(sv)
      if(length(sv) > M){  #M
        res = c()
        klm = c()
        for(i in length(sv)){
          klm = c(klm, gaussian_k(xi=spam[sv[i], 1:48], sv= sv))
          m = y[sv[i]]*(yp[sv[i]]-1*y[sv[i]]*klm[i])
          res = c(res, m)
        }
        res
        index = which.max(res)
        
        sv = sv[-index] 
        #print(sv)
      }
    }
    sv
  }
  errorrate
  sv
  list = list(errorrate = errorrate,  sv = sv)
  return(list)
}




res1 = online_learning(M=500, beta=0)
res2 = online_learning(M=500, beta=-0.05)
res3 = online_learning(M=20, beta=0)
res4 = online_learning(M=20, beta=-0.05)

plot(res1$errorrate[seq(from=1, to=500, by=10)], ylim=c(0,1), type="o", col = "red")
lines(res2$errorrate[seq(from=1, to=500, by=10)], type="o", col = "black")
lines(res3$errorrate[seq(from=1, to=500, by=10)], type="o", col = "green")
lines(res4$errorrate[seq(from=1, to=500, by=10)], type="o", col = "blue")
legend("topright",col=c("red","black","green","blue"), pch=1, 
       legend=c("M=500, beta=0","M=500, beta=-0.05","M=20, beta=0","M=20, beta=-0.05"))

N = 500
length(res1$sv)
res1$errorrate[N]

length(res2$sv)
res2$errorrate[N]

length(res3$sv)
res3$errorrate[N]

length(res4$sv)
res4$errorrate[N]

