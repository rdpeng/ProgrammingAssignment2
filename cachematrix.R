## The functions objective are to cache the inverse of a matrix

## Creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    #Set the value of matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    #Get the value of matrix
    get <- function() x
    
    #Set the value of inverse
    setInverse <- function(inverse) i <<- inverse
    
    #Get the value of inverse
    getInverse <- function() i
    
    #Return special matrix
    list (set = set, 
          get = get, 
          setInverse = setInverse, 
          getInverse = getInverse)
}


## Computes the inverse of the special matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
