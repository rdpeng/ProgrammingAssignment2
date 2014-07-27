makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    # get() returns the matrix encapsulated by the object
    get <- function() x
    # set() clears both the data and the cached inverse
    set <- function(y) {
        x <<- y
        inv  <<- NULL
    }
    # getInverse() returns the cached inverse
    getInverse <- function() inv
    # setInverse() saves the inverse to the cache
    setInverse <- function(inverse) inv <<- inverse
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}
su

cacheSolve <- function(x, ...) {
    # return the inverse from the cache if it has been cached already
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    # calculate the inverse, cache it, and return it
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
