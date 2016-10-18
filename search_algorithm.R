# binary search
binary_search <- function(A, key){
  imin = 1
  imax = length(A)
  
  while(imin <= imax){
    #print(imin)
    #print(imax)
    imid = round(imin + (imax - imin)/2)
    #print(imid)
    if(A[imid] == key) return(imid)
    else if(A[imid] < key) imin = imid + 1
    else imax = imid - 1
  }
  
  return(NA)
}

A = 10:19
binary_search(A, 19)
binary_search(A, 11)
binary_search(A, 5)

# linear search
linear_search <- function(A, key){
  for(i in 1:length(A)){
    if(A[i] == key) return(i)
  }
  return(NA)
}

linear_search(A, 19)
linear_search(A, 11)
linear_search(A, 5)

BA = 1:1e6
system.time(binary_search(BA, 91281))
system.time(linear_search(BA, 91281))

system.time(binary_search(BA, 0))
system.time(linear_search(BA, 0))

