## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## Contains cache for inverse
  cachedInv <- NULL
  
  ## Assigns new matrix and reset cache
  set <- function(y) {
    x <<- y
    cachedInv <<- NULL
  }
  
  ## Get current matrix
  get <- function() x
  
  ## Set cache for matrix
  setInverse <- function(inv) cachedInv <<- inv
  
  ## Get current cache
  getInverse <- function() cachedInv
  
  ## Return list of functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    ## Get cache of x
    inv <- x$getInverse()
    ## Return cache data
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    ## Calculate inverse and cache it
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
