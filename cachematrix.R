## This R file is going to cache the inverse of a matrix, and is largely
## based on the example of caching the mean of a vector in guidance.
## Thanks for your reviewing!


## Create a list containing a function to set and get the matrix 
## and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x   <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## Calculate the inverse of the matrix if not existing

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached inverse")
        return(m)
    }
    mtx <- x$get()
    m    <- solve(mtx, ...) 
    x$setinv(m)
    m
}

