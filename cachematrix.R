
## the function below caches the matrix 'x'

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


## this function returns a matrix that is the inverse of 'x'


cacheSolve <- function(x, ...) {
  invrs <= x$getinverse()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  mtx <- x$get()
  invr <- solve(mtx, ...)
  x$setinverse(invrs)
  invrs
}

