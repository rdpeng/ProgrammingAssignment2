## Put comments here that give an overall description of what your
## functions do

## This function makes a matrix that can cache an inverse of itself

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

## This function retrieves the cached inverse


cacheSolve <- function(x, ...) {
  ## Returns cached inverse for x
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cache")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
