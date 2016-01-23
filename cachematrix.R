## This functions works at the same way of the example with cacheMean
## and makeVector. The only difference was the internal function
## 'mean' it was replaced by 'solve' function.

## Function to build a matrix and all sub-functions to save,
## get and calculate his inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## Function that verify if the solve was executed or not.
## If the solve of matrix were already executed then retrieve
## from the cached result

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached matrix")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}
