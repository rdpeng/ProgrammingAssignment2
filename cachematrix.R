## The two functions below coincide to build a Matrix and cache the
## inverse for our assignment

## This function creates the Matrix that caches its inverse that 
## will be used by cacheSolve

makeCacheMatrix <- function(j = matrix()) {
  inv <- NULL
  set <- function(o) {
    j <<- o
    inv <<- NULL
  }
  get <- function() j
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function calculates the inverse of our previously created matrix.

cacheSolve <- function(j, ...) {
  ## Return a matrix that is the inverse of 'j'
  inv <- j$getInverse()
  if (!is.null(inv)) {
    message("grabbing cached data")
    return(inv)
  }
  mat <- j$get()
  inv <- solve(mat, ...)
  j$setInverse(inv)
  inv
}
