## Here are a pair of functions that cache the inverse of a matrix.
## Because matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.
## It's assumed that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function()x
  setsolve <- function(solve) s <<- solve
  getsolve <- function()s
  list(set = set, get = get,
       setsolve = setsolve, 
       getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve would retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
