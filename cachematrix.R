## Functions that cache the inverse of a matrix

## create a "special" matrix which is a list be able to 
## set/get values of matrix, set/get values of inverse matrix

makeCacheMatrix <- function(x = matrix(),...) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculate the inverse of the special "matrix" created with the above
## function, reusing cached result if it is available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    m <- x$get()
    m <- solve(m, ...)
    x$setinverse(m)
    m
}
