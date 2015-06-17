# This document is writtent according to Google's R Style Guide
# https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml
# except indetation rules, because in cource lectorer insisted
# on 4 or 8 spaces indetation.
#
# No extra arguments are passed into cacheSolve because otherwise
# cache will be invalidated in case if different parameters
# are passed.

makeCacheMatrix <- function(x = matrix()) {
    # Create matrix with it's inverse being cached.
    #
    # Args:
    #   x - input argument
    #
    # Returns:
    #   "Object" that that keeps cacked information
    inv <- NULL
    set <- function(y) {
       x <<- y
       inv <<- NULL
    }
    get    <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set        = set,
         get        = get,
         setinverse = setinv,
         getinverse = getinv)
}


cacheSolve <- function(x) {
    # Execute solve with cached result.
    #
    # Args:
    #   x - cache matrix object
    #
    # Returns:
    #   Matrix inverse.
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv  <- solve(data)
    x$setinverse(inv)
    inv
}
