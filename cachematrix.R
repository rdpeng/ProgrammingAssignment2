##If the contents of a matrix are not changing, 
##it may make sense to cache the value of the INVERSE so that when we need it again,
##it can be looked up in the cache rather than recomputed.

##The function "makeCacheMatrix" creates a special "vector", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse matrix
##get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse  <<- NULL
    }
    get <- function() x
    setinv <- function(z) inverse <<- z
    getinv <- function() inverse  
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##The "cacheSolve" function calculates the INVERSE of the special "vector" created with the above function. 
##However, it first checks to see if the INVERSE has already been calculated. 
##If so, it gets the INVERSE from the cache and skips the computation. 
##Otherwise, it calculates the INVERSE of the data and 
##sets the value of the INVERSE in the cache via the setinv function.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    iMx <- x$getinv ()
    if(!is.null(iMx)) {
        message("getting cached data")
        return(iMx)
    }
    data <- x$get()
    iMx  <- solve(data, ...)
    x$setinv(iMx)
    iMx
}
