## Vincent Davis | davis.vince@gmail.com
## This R script will implement a method to save users a computationally
## intensive function: calculating the inverse of a matrix.  This will be
## achieved by looking for a cached version of the results prior to actually
## computing the inverse calculations.  The functions to do this are based
## on a template provided and can be adapted to use the same caching
## approach for a wide variety of computationally intensive calculations

## The makeCacheMatrix function sets the value of the matrix, gets the
## value of the matrix, sets the value of the inverse (solve) and gets the
## value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)  
}

## The cacheSolve function computes the inverse of the matrix, but only
## after first checking to see if the inverse has already been calculated. 
## If the inverse of the matrix has already been cached, the cached
## version is returned.  Otherwise, the inverse is computed.

cacheSolve <- function(x, ...)  {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
