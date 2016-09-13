
my_magic_list <- function(){
  list(info="my own list", 
       c(1.04139,0.80902,2.84965,0.21053),
       matrix(c(4,9,2,3,5,7,8,1,6), 
              nrow=3, ncol=3, byrow = TRUE)
      )
}

# 1.3.4
# sum together all numeric elements in the list
sum_numeric_parts <- function(x){
  r = 0
  for(member in x){
    value = sum(as.numeric(member))
    if(is.na(value)){
      next
    }else{
      r = r + value 
    }
  }
  r
}

#a_list = my_magic_list()
#sum_numeric_parts(a_list)
#sum_numeric_parts(x = a_list[2])


# 1.4.1
my_data.frame <- function(){
  data.frame(id=c(1, 2, 3), 
             name=c("John", "Lisa", "Azra"),
             income=c(7.30, 0.00, 15.21),
             rich=c(FALSE, FALSE, TRUE))
}

#my_data.frame()


# 1.4.2
sort_head <- function(df, var.name, n){
  head(df[order(-df[var.name]), ], n)
}

#data(iris)
#sort_head(df = iris, var.name = "Petal.Length", n = 5)


# 1.4.3
add_median_variable <- function(df, j){
  df[ ,"compared_to_median"] = "Median"
  m = median(df[[j]])
  for(i in 1:nrow(df)){
    if(df[i,j] > m){
      df[i,"compared_to_median"] = "Greater"
    }else if(df[i,j] < m){
      df[i,"compared_to_median"] = "Smaller"
    }else{
      next
    }
  }
  df
}

#data(faithful)
#head(add_median_variable(df = faithful, 1))
#tail(add_median_variable(df = faithful, 2))

# 1.4.4
analyze_columns <- function(df, j){
  # fetch vectors for two columns
  col1 = df[[j[1]]]
  col2 = df[[j[2]]]
  
  #fetch coloum names
  col1_name = colnames(df)[j[1]]
  col2_name = colnames(df)[j[2]]
  
  # create correlatoin matrix and assgin names
  cm = cor(data.frame(col1, col2))
  colnames(cm)[1] = col1_name
  colnames(cm)[2] = col2_name
  rownames(cm)[1] = col1_name
  rownames(cm)[2] = col2_name

  # create list and assign names
  rl = list(c(mean=mean(col1), median=median(col1), sd=sd(col1)),
       c(mean=mean(col2), median=median(col2), sd=sd(col2)),
       correlation_matrix=cm
       )
  names(rl)[1] = col1_name
  names(rl)[2] = col2_name
  
  rl
}

analyze_columns(df = faithful, 1:2)
analyze_columns(df = iris, c(1,3))
analyze_columns(df = iris, c(4,1))



