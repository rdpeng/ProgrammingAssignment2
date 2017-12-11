## This makeCacheMatrix function creates a special "matrix" object that can cache its inverse. 
## which is really in a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
library(matlib)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(matrix) m <<- Inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix_data <- x$get()
  m <- Inverse(matrix_data, ...)
  if(identical(m, x)){
    message("getting inversed cached matrix")
    return(m)
  }
  x$setinverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
        
