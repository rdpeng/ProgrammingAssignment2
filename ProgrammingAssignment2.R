## This function serves to cache the inverse of a matrix
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x  
  
  ## Matrix Inverse
  
  setInverse <- function(inverse) m  <<- inverse
  getInverse <- function() m 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  }

## Function to solve for the inverse
## first determines if inverse has already been calculated and cached
## if not calculates inverse using solve()
## if already calculate retrieves cached inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
      message("Getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
  
  }