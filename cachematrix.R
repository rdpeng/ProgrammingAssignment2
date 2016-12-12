
## Caching the Inverse of a Matrix:
## Below are functions that are used to create a special object that first stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.


## This function computes the inverse of the special "matrix" created by makeCacheMatrix below. If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.

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


## cacheSolve is a function which computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Retrieving cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

