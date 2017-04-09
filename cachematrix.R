
## this function sets up the matrix

makeCacheMatrix <- function(x=matrix(rnorm(16),4,4)){
  inv <- NULL
  b <- x
  set <- function(y){
    b <<- y
    inv <<- NULL
  }
  get <- function() b
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## this function computes the inverse of the matrix

cacheSolve <- function(x){
  inv <- x$getInverse()
  if(!is.null(inv) && x$get()==b){
    message("getting cached inverse")
    return(inv)
  }
  mata <- x$get()
  inv <- solve(mata)
  x$setInverse(inv)
  inv
}