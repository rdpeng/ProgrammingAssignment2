## Have modified the example to enable the functionality of storing the inverse of a matrix in the cache
## The makeCacheMatrix function defines the get and set functions for the matrix variable and its mean
##cacheSolve functions helps in getting the mean from cache or calculating a new mean

makeCacheMatrix <- function(x = matrix()) {
  r <- NULL
  set <- function(y) {                                    ##resetting the matrix and the inverse
    x <<- y
    r <<- NULL
  }
  get <- function() x
  setReverse <- function(reverse) r <<- reverse                 
  getReverse <- function() r
  list(set = set, get = get,
       setReverse = setReverse,
       getReverse = getReverse)
}

cacheSolve <- function(x, ...) {
  r <- x$getReverse()                                         ##getting stored value of reverse
  if(!is.null(r)) {                                           ##checking if stored value is present in cache or not
    message("getting cached data")
    return(r)
  }
  data <- x$get()
  r <- solve(data, ...)                                       ##calculating inverse of matrix
  x$setReverse(r)
  r
}
          
