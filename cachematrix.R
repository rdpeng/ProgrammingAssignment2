## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Prepares an object which can be used by cacheSolve() to
# efficiently retrieve the inverse of a matrix once the
# cache is initialized.

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv_x <<- inv
  getinv <- function() inv_x
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

# CacheSolve will return the inverse of matrix x assumed 
# to be a square invertable matrix.  The method is efficient
# in that the inverse is cached and subsequent calls will 
# return the cached inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinv()
  if(!is.null(inv_x)) {
    message("getting cached data")
    return(inv_x)
  }
  data <- x$get()
  inv_x <- solve(data,...)
  x$setinv(inv_x)
  inv_x
}
