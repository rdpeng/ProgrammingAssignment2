# This two functions cache the inverse of a matrix, assuming that the matrix supplied is always invertible
#
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# makeCacheMatrix is a list of functions:
#   set: set the value of the matrix
#   get: get the value of the matrix
#   setinvers: calculate the inverse of the matrix
#   getinvers: return the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invers <- NULL
  set <- function(y) {
    x <<- y
    invers <<- NULL
  }
  get <- function()
    x
  setinvers <- function(solve)
    invers <<- solve
  getinvers <- function()
    invers
  list (
    set = set, get = get, setinvers = setinvers, getinvers = getinvers
  )
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invers <- x$getinvers()
        ## If the inverse has already been calculated, return it from the cache
  if (!is.null(invers)) {
    message("getting cached data")
    return(invers)
  }
       ## If the inverse is not already calculated, calculate it and set the inverse value in the cache
  data <- x$get()
  invers <- solve(data, ...)
  x$setinvers(invers)
  invers
  
}
