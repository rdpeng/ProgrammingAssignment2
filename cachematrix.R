## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a matrix to cach it's reverse.

makeCacheMatrix <- function(x = matrix()) {

    omg <- NULL
    set <- function(y) {
        x <<- y
        omg <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) omg <<- inverse
    getInverse <- function() omg
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## Write a short comment describing this function
## This function calculates the inverse of the matrix created
## above. If the inverse isalready calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    omg <- x$getInverse()
    if (!is.null(omg)) {
        message("getting cached data")
        return(omg)
    }
    mat <- x$get()
    omg <- solve(mat, ...)
    x$setInverse(omg)
    omg
}
