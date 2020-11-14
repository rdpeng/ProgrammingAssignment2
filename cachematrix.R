makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <-function()x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() {
    inver <- ginv(x)
    inver%*%x
  }
  list(set = set, get = get, setInverse = setInverse, getinverse = getInverse)
}
cacheSolve <- function(x, ...){
  inv <- x$getInverse
  if(!is.null(inv)){
    message("Cashed Data")
    return(inv)
  }
  mat <- x$get()
  inv <-solve(mat, ...)
  x$setInverse(inv)
  inv
}
