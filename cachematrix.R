## Put comments here that give an overall description of what your
## functions do

## Caching the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function( matrix ) {
    x <<- matrix
    inv <<- NULL
  }
  get <- function() {
    x 
  }
  setInverse <- function() {
    #calculate the inverse
    inv <<- solve(x) 
  }
  getInverse <- function() {
    inv
  }
  
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x <- x$getInverse()
  
  if( !is.null(x) ) {
    message("getting cached data")
    return(x)
  }
  
  data <- x$get()
  x <- solve(data) %*% data
  x$setInverse(x)
  x
}
