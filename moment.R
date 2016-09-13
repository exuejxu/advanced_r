a = function(x,i) {
  mx <- mean(x)
  len = length(x)
  j = 1
  sum = 0
  for(j in 1:length(x)) {
    mm = x[j] - mx
    mn = mm^i
    sum = sum + mn
    j = j + 1
  }
  cm = sum / len
  cm
}


a(1:100, 2)

#1.7 Clojures
#1.7.1 moment()
moment <- function(i) {  #x,returns another function which adds its argument
  function(x) {
    mx <- mean(x)
    len = length(x)
    j = 1
    sum = 0
    for(j in 1:length(x)) {
      mm = x[j] - mx
      mn = mm^i
      sum = sum + mn
      j = j + 1
    }
  }
  cm = sum / len
  cm
}


m1 <- moment(i=1)
m1(1:100)

m2 <- moment(i=2)
m2(1:100)