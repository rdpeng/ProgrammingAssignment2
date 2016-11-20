## Caching the Inverse of a Matrix:
## Matrix inversion is a computation heavy activity.
## The primary benefit to caching the inverse of a matrix rather than computing it repeatedly.
## is the possibility of deriving efficiency in the code along with increased speed.
## Below are a pair of functions that are used to create an object that 
## stores a matrix and caches its inverse.

## This function creates a matrix object that can cache its inverse.

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


## This function cahces the inverse of the matrix created by the makeCacheMatrix function above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("grabbing cache")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
