## Save time by only calculating an inverse once. 
## The function makeCacheMatrix and cacheSolve work together.

## Input a matrix and it will create a list of functions that can be called
##to set & get the matrix, set & get the inverse

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    
  }
  
  get <- function() x
  setinverse <<- function(solve) m <<- solve
  getinverse <<- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## The first time this is called it will calculate the inverse and store it in the cache
## If makeCacheMatrix hasn't been called, it will retrieve the value from the cache on subsequent calls
## if makeCacheMatrix is called again, the value in the cache will be set to NULL
## and the next time you call cacheSolve it will recalculate the inverse.

cacheSolve <- function(x, ...) {
  
  m <<- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

