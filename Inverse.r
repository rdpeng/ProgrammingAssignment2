## Caching the Inverse of a Matrix:

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  INV <- NULL
  set <- function(y) {
    x <<- y
    INV <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) INV <<- inverse
  getInverse <- function() INV
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  INV <- x$getInverse()
  if (!is.null(INV)) {
    message("getting cached data")
    return(INV)
  }
  mat <- x$get()
  INV <- solve(mat, ...)
  x$setInverse(INV)
  INV
}
