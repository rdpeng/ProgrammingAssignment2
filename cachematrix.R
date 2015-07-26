## makeCacheMatrix and cacheSolve create a matrix and can cache and return
## its inverse

## makeCacheMatrix includes four functions to create a matrix object and cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    inverse_x <- NULL
    set <- function(y) {
        x <<- y
        inverse_x <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse_x <<- inverse
    getinverse <- function() inverse_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## cacheSolve returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

    inverse_x <- x$getinverse()
    if(!is.null(inverse_x)) {
        message("getting cached data")
        return(inverse_x)
    }
    data <- x$get()
    inverse_x <- solve(data, ...)
    x$setinverse(inverse_x)
    inverse_x
    
}
