## Inversing a matrix is resource intensive and caching the 
## results prevents reprocessing

## this function creates a matrix and caches the reults of
## inversing the matrix.

makeCacheMatrix <- function(a = matrix()) {
  invrs <- NULL
  set <- function(y) {
    a <<- y
    invrs <<- NULL
  }
  get <- function() a
  setInverse <- function(inverse) invrs <<- inverse
  getInverse <- function() invrs
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return the inverse of the matrix.

cacheSolve <- function(a, ...) {
  ## Return a matrix that is the inverse of 'a'
  invrs <- a$getInverse()
  if (!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  mat <- a$get()
  invrs <- solve(mat, ...)
  a$setInverse(invrs)
  invrs
}
