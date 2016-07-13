## makeCacheMatrix is for creating a special matrix
## cacheSolve is meant for calculating inverse of special matrix created by makeCacheMatrix
## makeCacheMatrix creates a namespace for mat and mat inverse and returns four functions for seeting and getting the values of those variables

makeCacheMatrix <- function(x = matrix()) {
        inv <- Null
        set <- function(y) {
                x <<- y
                inv <<- Null
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
}

## In the second function, it computes the inverse of the special matrix returned by the first function, 
 ## if the inverse has been calculated, then the cachesolve should retreive the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
   # if the inverse has already been calculated
  if (!is.null(inv)) {
  # get it from the cache and skips the computation.
    message("getting the cached data")
    return(inv)
  }
  # otherwise, calculates the inverse
  mat <- x$get()
  inv <- solve(mat, ...)
  # sets the value of the inverse in the cache via the setinv function.
  x$setInverse(inv)
  inv
}
