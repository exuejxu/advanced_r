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




