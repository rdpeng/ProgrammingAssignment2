## This function is created to cache the inverse of a matrix
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix
##rather than compute it repeatedly.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This cacheSolve function will be he one responsible to solve for the inverse

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
