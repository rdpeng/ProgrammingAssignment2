## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(x) {
        mtx <<- x;
        inverse <<- NULL;
    }
        get <- function() return(mtx);
        setinv <- function(inv) inverse <<- inv;
        getinv <- function() return(inverse);
        return(list(set = set, get = get, setinv = setinv, getinv = getinv))

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- mtx$getinv()
        if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
        data <- mtx$get()
        invserse <- solve(data, ...)
        mtx$setinv(inverse)
        return(inverse)
}
