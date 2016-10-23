## This are 2 functions that cache the inverse of a function. The first one 
## makes the cache, the second one return the invers.

## This function makes the cache of a matrix.

makeCacheMatrix <- function(x = matrix()) {
            
      inv <- NULL
      
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      get <- function() x
      
      setinv <- function(solve) inv <<- solve
      
      getinv <- function() inv
            
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function calculates the inverse of the matrix if it is not calculated yet or the matrix has changed
## otherwise it returns the cache if it has been calculated before.

cacheSolve <- function(x, ...) {
      
            m <- x$getinv()
            
            if(!is.null(m)) {
                  message("getting cached data")
                  return(m)
            }
            
            data <- x$get()
            m <- solve(data, ...)
            x$inv(m)
            m
}
