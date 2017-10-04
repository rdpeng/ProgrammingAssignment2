
## These functions create a matrix, compute the inverse and then make use 
## of the <<- operator to create a special object that allows the result of the 
##inverse calculation to be pulled from cache assuming the matrix has not changed. 

## Uses the <<- operator to assign a value to an object in an environment separate from
## the current environment. It includes two functions that are used to create a special
## object that creates a matrix and caches the inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
  }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  
}


## This function finds the inverse of the matrix created in makeCache Matrix.
## if the calculation has already been done, it gets the result from the cache.

cacheSolve <- function(x, ...) {
     inv <- x$getInverse()
     if(!is.null(inv)) {
        message("Showing Cached Data")
        return(inv)
  }
  data<- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
  
}