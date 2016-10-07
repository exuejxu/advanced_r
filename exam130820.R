# 1. Data and programming structure
computer = list(brand=c('HP','Asus','Dell','Apple'),
                price=c(12000,11500,10300,13100),
                speed=c(100.6,97.5,101.8,95.0),
                Linux=c(FALSE, FALSE, FALSE, TRUE))

speedorder = order(computer$speed, decreasing=FALSE)
for(i in names(computer)) {
  computer[[i]] = computer[[i]][speedorder]
}
computer


# 3. Strings and input/output
findword <- function(filename, word){
  filestr = readChar(fileName, file.info(fileName)$size)
  filestrv = unlist(strsplit(filestr, split=c(' ', '\r', '\n')))

  count = 0
  for(w in filestrv){
    if(w == word) count = count + 1
  }
  
  cat(word, "has occured ", count, "times in ", filename)
}

findword('test.txt', "hello")

# 4. Interfacing and parallel computations
library(parallel)

max_row <- function(X){
  # Calculate the number of cores
  no_cores <- detectCores()
  
  # Initiate cluster
  cl <- makeCluster(no_cores)
  
  r = parApply(cl, x, 1, max)
  
  # Shut down cluster
  stopCluster(cl)
  
  r
}

x = matrix(c(1,3,4,5), nrow=2, ncol=2)
max_row(x)




