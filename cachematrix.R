## Caching the Inverse of a Matrix:

## Below are a pair of functions that are used to create a special object that
## stores a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
  ## This list is used as the input to cacheSolve()
}


## cacheSolve() computes the inverse of the special "matrix" created by 
## makeCacheMatrix() above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  ## if the inverse has already been calculated
  
  if(!is.null(inv)){
    message("getting cached data")
    
        ## Get it from the cache and skip the computation
        ## Return a matrix that is the inverse of 'x'
    
    return(inv)
  }
  
  ## Otherwise, calculate the inverse
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
