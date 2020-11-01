## Put comments here that give an overall description of what your
## functions do

## There are two functions makeCacheMatrix and CacheSolve
##makeCahceMatrix consists of set, get, setinv and getinv
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ##initializing inverse as NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}  ##function to get matrix x
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}  ##function to inverse of matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
##this is used to get the cachedata

cacheSolve <- function(x, ...) ##gets cache data
  {
  inv <- x$getInverse()
  if(!is.null(inv)){           ##checking weather inverse is NULL
    message("getting cached data")
    return(inv)                ##returns inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...)       ##calculates inverse value
  x$setInverse(inv)
  inv
}
