## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: creates a matrix object that can cache its 
## inverse using cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  ## 'x': matrix to be inverted.  By definition of the problem, x is
  ## an invertible matrix
    i <- NULL
    set <- function(y){
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: takes the results of makeCacheMatrix and returns a 
## matrix that is the inverse of 'x'.
## Calculates the inverse if it does not exist and caches it.  
## Otherwise returns cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
