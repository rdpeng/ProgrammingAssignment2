## Caching the Inverse of a Square Invertible Matrix:
## Caching primarily helps to save the time of the computations when it is re-run.
## In this case, a pair of functions would help in creating a "special" square invertible
## matrix, computing its inverse and if the matrix stays unchanged, retrieves the inverse
## from the cache.

## This function creates the square invertible matrix that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
        
                # use '<<-' to assign value to an object in an environment different from 
                # the current environment
                  x <<- y
                  inv <<- NULL
     }
   get <- function() x
   setInverse <- function(inverse) inv <<- inverse
   getInverse <- function() inv
   list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
   }
   
## This function computes the inverse of the matrix created by the above function and if the 
## matrix stays unchanged, retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        
        # If the inverse has been already calculated
        if (!is.null(inv)) {
        # Get the cached data (from the cache) without any computations
                   message("Get cached Data")
                   return(inv)
     }
     
       # Else, calculate the inverse.
   mat <- x$get()
   inv <- solve(mat, ...)
   
   # This setInverse function sets the inverse value in the cache.
   x$setInverse(inv)
   inv
  }
    
