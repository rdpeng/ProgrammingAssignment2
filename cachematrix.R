## Below are two functions that are used to create a special object that
## stores a matrix and caches its inverse.

## makeCacheMatrix returns a list of functions 
## setMatrix and getMatrix are to set and get the matrix X
## setInverse and getInverse are to set and get the inverse of matrix X

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMatrix <- function() x
  
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  
  list(setMatrix = setMatrix, 
  	   getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Return a matrix that is the inverse of 'x':
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse 
## in the cache via the setInverse function

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getMatrix()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}