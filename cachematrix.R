## A pair of functions that cache the inverse of a matrix.

## makeCacheMatrix function creates a matrix object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inversed <- NULL
    set <- function(y) {
        x <<- y
        inversed <<- NULL
    }
    get <- function() x
    setinversed <- function(solve) inversed <<- solve
    getinversed <- function() inversed
    list(set = set, get = get,
         setinversed = setinversed,
         getinversed = getinversed)
}


## cacheSolve function computes the inverse of the matrix returned
## by the makeCacheMatrix above. If the inverse has
## already been calculated the cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inversed <- x$getinversed()
    if(!is.null(inversed)) {
        message("getting cached data")
        return(inversed)
    }
    data <- x$get()
    inversed <- solve(data, ...)
    x$setinversed(inversed)
    inversed
}
