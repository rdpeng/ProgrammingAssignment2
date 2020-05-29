##makeCacheMatrix:
##This function creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##cacheSolve:
##This function computes the inverse of the special “matrix” returned by makeCacheMatrix above.
##Gets relult of computation when already done, otherwise, it computes the inverse
##and retrieve it from the cache

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setInverse(i)
  i
}
