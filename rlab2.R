# 1.1.1
sheldon_game <- function(player1, player2){
    if(player1==player2){
      print("Draw!")
    }else if(player1 == "scissors"){
      if((player2 == "paper") || (player2 == "lizard")){
        print("Player 1 wins!")
      }else{
        print("Player 2 wins!")
      }
    }else if(player1 == "paper"){
      if((player2 == "rock") || (player2 == "spock")){
        print("Player 1 wins!")
      }else{
        print("Player 2 wins!")
      }
    }else if(player1 == "rock"){
      if((player2 == "lizard") || (player2 == "scissors")){
        print("Player 1 wins!")
      }else{
        print("Player 2 wins!")
      }
    }else if(player1 == "lizard"){
      if((player2 == "spock") || (player2 == "paper")){
        print("Player 1 wins!")
      }else{
        print("Player 2 wins!")
      }
    }else if(player1 == "spock"){
      if((player2 == "scissors") || (player2 == "rock")){
        print("Player 1 wins!")
      }else{
        print("Player 2 wins!")
      }
    }else{
    stop("invalid arguments!")
  }
}

#sheldon_game("lizard", "spock")
#sheldon_game("rock", "paper")

# 1.2.1
my_moving_median <- function(x, n, na.rm=FALSE){
  if(!is.numeric(x) || !is.numeric(n)){
    stop("the vector and scalar must be numeric!")
  }
  
  # initiate the return vector
  rv = c()
  
  # for each sub-vector
  for (i in 1:(length(x)-n)) {
    tv = x[i:(i+n)]
    m = median(tv, na.rm)
    if(is.na(m)){
      rv = c(rv, NA)
    }else{
      if(any(is.na(tv))){
        rv = c(rv, m)
        next
      }else {
        for (value in tv) {
          if(value == m){
            rv = c(rv, value)
            break
          }
        } 
      }
    }
  }
  rv
}

#my_moving_median(x = 1:10, n=2)
#my_moving_median(x = 5:15, n=4)
#my_moving_median(x = c(5,1,2,NA,2,5,6,8,9,9), n=2)
#my_moving_median(x = c(5,1,2,NA,2,5,6,8,9,9), n=2, na.rm=TRUE)


# 1.3.3
trial_division_factorization <- function(x){
  rv = c()
  if(x<2){
    return(rv)
  }
  
  for(p in 2:floor(sqrt(x))){
    if(p*p > x) {
      break
    }
    while((x %% p) == 0){
      rv = c(rv, p)
      x = x %/% p
    }
  }
  
  if(x > 1){
    rv = c(rv, x)
  }
  return(rv)
}

#trial_division_factorization(x = 2^3 * 13 * 17 * 31)
#trial_division_factorization(x = 47 * 91 * 97)


# 1.5.1
in_environment <- function(env){
  ls(as.environment(env))
}

#env = search()[length(search())]
#env
#funs <- in_environment(env)
#funs[1:5]


# 1.5.2
where <- function(fun){
  if(length(fun) > 1){
    stop("invalid argument!")
  }
  
  paths = search()
  for(path in paths){
    env = as.environment(path)
    if(exists(fun, env, inherits = FALSE)){
      return(path)
    }
  }
  
  sprintf("%s not found!", fun)
}

#where(fun = "sd")
#where(fun = "read.table")
#where(fun = "non_existant_function")

# 1.7.1
moment <- function(i){
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
    cm = sum / len
    cm
  }
}

#m1 <- moment(i=1)
#m2 <- moment(i=2)
#m1(1:100)
#m2(1:100)

# 1.7.2
mcmc_counter_factory <- function(burnin, thin){
  iteration = 0
  samples = 0
  
  function(){
    iteration <<- iteration + 1
    store_sample = FALSE
    
    if(iteration > burnin){
      if((iteration-burnin) %% thin == 0){
        store_sample = TRUE
        samples <<- samples + 1
      }
    }
    
    list(iteration, store_sample, samples)
  }
}

mcmccnt <- mcmc_counter_factory(burnin = 3, thin = 2)
mcmccnt()
mcmccnt()
mcmccnt()
mcmccnt()
mcmccnt()
mcmccnt()


