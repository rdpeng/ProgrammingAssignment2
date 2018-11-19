## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y;
        inverse <<- NULL;
    }
    get <- function() return(x);
    set_inv <- function(i) inverse <<- i;
    get_inv <- function() return(inverse);
    return(list(set = set, get = get, set_inv = set_inv, get_inv = get_inv))
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inverse <- x$get_inv()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- x$get()
    invserse <- solve(data, ...)
    x$set_inv(inverse)
    return(inverse)
}
