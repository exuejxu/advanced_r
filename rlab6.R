set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

# 1.1.2 Brute force search
# wiki method
brute_force_knapsack <- function(x, W){
  n = nrow(x)
  
  #initilize m, use 1st col and row as "0"
  m = data.frame(matrix(nrow=n+1, ncol=W+1))
  m[1, ] = 0
  
  for(i in 1:n){
    ii = i + 1
    for(j in 0:W){
      jj = j + 1
      if(x[i, ]$w > j)
        m[ii,jj] = m[ii-1, jj]
      else
        m[ii,jj] = max(m[ii-1,jj], m[ii-1, jj-x[i, ]$w] + x[i, ]$v)
    }
  }
  

  # get elements
  v = c()
  for(i in 1:n){
    v[i] = 0
  }
  
  j = W + 1
  i = n + 1
  while(i > 1){
    if(m[i, j] > m[i-1, j]){
      v[i-1] = 1
      j = j - x[i-1, ]$w
    }
    
    i = i - 1
  }
  
  ev = c()
  for(i in 1:n){
    if(v[i] == 1) ev = c(ev, i) 
  }
  
  list(value=round(m[n+1, W+1]), elements=ev)
  
}

# lab specified method
brute_force_knapsack1 <- function(x, W){
  n = nrow(x)
  maxv = 0
  ev = c()
  
  for(i in 0:(2^n-1)){
    iv = intToBits(i)
    sv = 0
    sw = 0
    tv = c()
    for(j in 1:n){
      if(iv[j] == 1){
        sv = sv + x[j, ]$v
        sw = sw + x[j, ]$w
        tv = c(tv, j)
      }
    }

    if((sv > maxv) && (sw <= W)){
      maxv = sv
      ev = tv
    }
  }
  
  list(value=round(maxv), elements=ev)
}


#brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
brute_force_knapsack1(x = knapsack_objects[1:12,], W = 3500)

