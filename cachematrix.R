
makeCacheMatrix <- function(x = matrix()) {
    # initialize the stored inverse value to NULL
    inverse <- NULL

    # to set the value of the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL   # since the matrix changed
    }
    # to get the value of the matrix
    get <- function() x
    # to set the inverse
    setinverse <- function(inverse_) inverse <<- inverse_
    # to get the inverse
    getinverse <- function() inverse

    # return a list of all the above functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}




cacheSolve <- function(x, ...) {
    # check if the inverse is already cached
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    # not cached, so we get the matrix into data
    data <- x$get()
    # and compute the inverse
    inverse <- solve(data, ...)
    # then cache the inverse
    x$setinverse(inverse)
    # and return it as well
    inverse
}
