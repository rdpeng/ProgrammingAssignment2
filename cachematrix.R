## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Creates a list containing to set/get the value of the matrix 
# set/get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      
      get <- function() x
      setInverse <- function(solve) m <<- solve
      getInverse <- function() m
      
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("Getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}

