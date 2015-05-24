


## This function takes a square invertible matrix x as input and returns a
## list of four functions: x$set, x$get, x$setinverse and x$getinverse

makeCacheMatrix <- function(x = matrix()) {
    ## input x is a square invertible matrix
    ## returns a list containing functions to set and get the matrix
    ## and functions to set and get the inverse matrix
    
    inv <- NULL
    set <- function(y) {
        # we use <<- to assign a value to an object in different environment
        # from the current environment
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

## This function takes an input with the same type as an output of the previous
## function and returns its inverse either from the cache (if it has been already
## calculated or set manually by using setinverse), or by solving it

cacheSolve <- function(x, ...) {
    ## input x for this function is of the same type as the output of makeCacheMatrix()
    ## returns the inverse of the original matrix either from the cache
    ## or by solve if there is no cached value
    inv <- x$getinverse()
    
    # if the inverse already exists in cache it prints the following message and that inverse
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    # otherwise calculates the inverse and returns it
    matr <- x$get()
    inv <- solve(matr, ...)
    x$setinverse(inv)
    return(inv)
}