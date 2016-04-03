## The following two functions cache the inverse of a matrix

## The following function creates a special matrix object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
            x <<- y
            inverse <<- NULL
    }
    get <- function() x
    setInv <- function(inv) inverse <<- inv
    getInv <- function() inverse
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## The following function computes the inverse of the special matrix
## retrurned by the makeCacheMatrix function above

cacheSolve <- function(x, ...) {
        inverse <- x$getInv()
        print (is.null(inverse))
        if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInv(inverse)
        inverse
}



