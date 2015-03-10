## Functions for matrix inversion with special feature
## of caching the inverse of a matrix rather than compute it repeatedly


## Creates get/set methods for matrix and inverted matrix
## assumption: the matrix supplied is always invertible
## no error handling in case of singular matrices

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Checks if object already has inverted matrix setted in "cache" (m)
## If true then get "cached data" from m and print it
## If false then invert matrix x, fill value of "cache" (m) and print it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
