#1.2.1 my moving median()
my_moving_median <- function(x, n, ...) {  
  if((is.numeric(x) == TRUE) && (is.numeric(n) == TRUE)) { #TRUE no need of ""
    
    # initiate the return vector
    moving <- c()
    
    len =length(x) - n
    for(i in 1:len){
      y = c(x[i:(i+n)])
      m = median(y, ...)
      moving <- c(moving,m)
    }
    moving
  }else{
    stop("the vector and scalar must be numeric!")
  }
}

my_moving_median(x = 1:10, n=2)
my_moving_median(x = 5:15, n=4)
my_moving_median(x = c(5,1,2,NA,2,5,6,8,9,9), n=2)
my_moving_median(x = c(5,1,2,NA,2,5,6,8,9,9), n=2, na.rm=TRUE)
