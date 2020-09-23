# 1 
ranStud <- function(seed, dfr, n){
  set.seed(seed)
  x <- rt(n, dfr)
  return(x)
}

# 2
tMtx <- function(initMtx, n){
  result <- matrix(NA, nrow = n, ncol = ncol(initMtx))
  if(is.matrix(initMtx) == F | any(is.integer(initMtx[1:2,] == T)) 
     | any(initMtx[2,] < 0) == T | nrow(initMtx) != 2){
    stop("Error: initMtx must contain integer values, and positive seeds")
  }else{
    for(i in 1:ncol(initMtx)){
      result[, i] <- matrix(ranStud(initMtx[1, i], initMtx[2, i], n))
    }
    return(result)
  }
}

# 4(a)
posCount <- function(v){
  count = length(which(v > 0))
  return(count)
}