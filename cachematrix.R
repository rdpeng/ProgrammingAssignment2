## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
## THe two functions below are used to storea matrix and cache its inverse

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invMatrix <<- inverse
  getInverse <- function() invMatrix
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  invMatrix <- x$getInverse()
  if (!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  mat <- x$get()
  invMatrix <- solve(mat, ...)
  x$setInverse(invMatrix)
  invMatrix
}

