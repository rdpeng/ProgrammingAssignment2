## This file constains two functions that create matrix and compute
## matrix inverse without replicated work. Everytime the inverse is
## calculated, it will be cached. The function will return cached 
## data at the next require of matrix inverse.

## This function makes a special matrix with cache feature

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    # set matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # get matrix
    get <- function() x
    # set inverse 
    setinv <- function(inverse) inv <<- inverse
    # get inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function calculate matrix inverse, if it has been cached,
## the cached data will be returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

