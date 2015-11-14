## This function returns a list of 4 founctions:
## set: create a square matrix
## get: get the square mastrix created
## setInverse: calculate the inverse of the matrix and cache it
## getInverse: read the cached inverse of the matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function solves the inverse of a matrix with cache
## If the inverse is cached,
##            it gets the inverse from cache directly
## Otherwise, 
##            it calculates the inverse, returns the result,
##            and store the result into the cache in the meantime

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
