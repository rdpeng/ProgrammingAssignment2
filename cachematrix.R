## makeCacheMatrix function
makeCacheMatrix <- function(x = matrix()) {
  res <- NULL
  set <- function(y){
    x <<- y
    res <<- NULL
  }
  get <- function() x
  setmatrix <- function(res.value)
  res <<- res.value
  getmatrix <- function()
  res
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}

##cacheSolve function
cacheSolve <- function(x, ...) {
  res <- x$getmatrix()
  if(!is.null(res)) {
    message("Cache found")
    return(res)
  }
  
  data <- x$get()

## Solve function
  solve <- function(data, ...){ 
    message("Cache not found . . .")
    message("inverse of matrix is . . .")
    
## new matrix to store temp matrix
    ans <- matrix(ncol=3, nrow=3)
    mod <- as.numeric(0)
    for(i in 1:3){
      cols <- 1:3
      temp <- cols[-i]
      i.fval <- temp[1]
      i.sval <- temp[2]
      for(j in 1:3){
        cols <- 1:3
        temp <- cols[-j]
        j.fval <- temp[1]
        j.sval <- temp[2]
        a <- data[i.fval, j.fval] * data[i.sval, j.sval]
        b <- data[i.sval, j.fval] * data[i.fval, j.sval]
        ans[i,j] <- a-b
        
##Finding modular of matrix
        if(i==1){
          suff <- data[i,j] * ans[i,j]
          if(j==2){
            suff <- (-1) * suff
          }
          mod <- mod + suff
        }
      }
    }

## changing sign of matrix
    for(i in 1:3)
      for(j in 1:3){
        if(((i==1 && j==2) || (i==2 && j==1) || (i==3 && j==2) || (i==2 && j==3) ) && ans[i,j]!=0)
          ans[i,j] <- (-1) * ans[i,j]
      }

## revesing matrix
    rev <- matrix(ncol = 3, nrow=3)
    for(i in 1:3)
      for(j in 1:3)
        rev[i,j] <- ans[j,i]
    
    return(rev/mod)
  }
  res <- as.matrix(solve(data,...), ncol=3, nrow=3)
  x$setmatrix(res)

## Returning result  
res
}
