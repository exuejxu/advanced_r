# 1. Data and programming structure
# a)
mymat = matrix(c(0,1), nrow=100, ncol=50)

myresfor = list(mean=c(), sd=c())
for(i in 1:nrow(mymat)){
  myresfor$mean = c(myresfor$mean, mean(mymat[i, ])) 
  myresfor$sd = c(myresfor$sd, sd(mymat[i, ]))
}
myresfor

myresappl = list(mean = apply(mymat, 1, mean),
                 sd = apply(mymat, 1, sd))
myresappl

if(myresfor$mean == myresappl$mean && myresfor$sd == myresappl$sd)
  print("values from both calculations are equal")

# b)
matrix_multiply <- function(m1, m2){
  n = nrow(m1)
  resultm = matrix(nrow=n, ncol=n)
  for(i in 1:n){
    for(j in 1:n){
      resultm[i,j] = m1[i,j] * m2[j,i]
    }
  }
  resultm
}

m1 = matrix(c(1,2,1,2), nrow = 2, ncol = 2)
m2 = matrix(c(3,4,3,4), nrow = 2, ncol = 2)
matrix_multiply(m1, m2)


# 2. Simulation, input/output and graphics
# a)
readinteger <- function(){ 
  myvalue <- as.integer(readline(prompt="Enter an integer: "))
  if(myvalue %% 3 == 0){
    cat(myvalue, "is evenly divisible by 3")
  }else if(myvalue %% 5 == 0){
    cat(myvalue, "is evenly divisible by 5")
  }else{
    cat(myvalue, "is not evenly divisible by 3 or 5")
  }
}
readinteger()







