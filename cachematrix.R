## Matrix inversion is usually a costly computation and there may be some benefit to caching the 
## inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix 
## inversion that we will not discuss here). This assignment comprises of a pair of functions that 
## caches the inverse of a matrix.

## This code creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv_m <<- inverse
  getInverse <- function() inv_m
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_m <- x$getInverse()
  if (!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m)
  }
  m <- x$get()
  inv_m <- solve(m, ...)
  x$setInverse(inv_m)
  inv_m
}

## <End of Script>
