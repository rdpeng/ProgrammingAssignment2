
## The aim of the folowing functions is to cache the
## inverse matrix calculation so that we can avoid
## multiple useless computations

## This function create a matrix that can have its 
## inverse cached

makeCacheMatrix <- function(x = matrix()) {
      
      ## initialise the inverse matrix
      inverse <- NULL
      
      ## define the getter and setter functions
      get <- function() x
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      
      ## define the functions for the inverse
      setInverse <- function(Inv) inverse <<- Inv
      getInverse <- function() inverse
      
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## This function returns the inverse of a matrix by first checking
## if it has not been computed before !

cacheSolve <- function(x, ...) {
      
      ## initialise the inverse matrix
      inverse <- x$getInverse()
      
      ## check the existence of a cached value
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      else {
            data <- x$get()
            inverse <- solve(data)
            x$setInverse(inverse)
            return(inverse)
      }
}
