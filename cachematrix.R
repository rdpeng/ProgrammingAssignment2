## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # i will store the inverse
    inv <- NULL

    # set should be used to alter the matrix
    # it invalidates the cache
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    # get simply returns the raw matrix
    get <- function() {
        x
    }

    # setinv sets the inv variable
    # should be used only by cacheSolve
    setinv <- function(i) {
        inv <<- i
    }

    # getinv gets the cached inverse
    getinv <- function() {
        inv
    }

    # return the special matrix
    list(set = set,get = get,setinv = setinv,getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    # get the cached inverse
    inv <- x$getinv()

    if(!is.null(inv)) {
        # if the inverse if actually cached, just return it
        message("getting cached inverse")
        return(inv)
    }

    # otherwise, calculate the inverse and cache it
    matr <- x$get()
    inv <- solve(matr, ...)
    x$setinv(inv)

    return(inv)
}
