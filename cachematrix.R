## This script checks to see if a matrix has been inverted before performing an inversion. 
## If the matrix has been inverted, it calls on the cached version instead
## of consuming time to compute the inversion again

## The following function - makeCacheMatrix - creates a matrix object.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## The next function - cacheSolve - computes the inverse of the matrix 
## created by the makeCacheMatrix function. If the inverse has already 
## been calculated then this function will call on the matrix from the cache
cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## The following returns the matrix that is the inverse of 'x'
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
