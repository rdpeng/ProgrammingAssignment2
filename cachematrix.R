## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        invmatx <- NULL
        set <- function(maty) {
                x <<- maty
                invmatx <<- NULL
        }
        get <- function() x
        setinv <- function(inv) invmatx <<- inv
        getinv <- function() invmatx
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmatx <- x$getinv()
        if(!is.null(invmatx)) {
                message("getting cached data")
                return(invmatx)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
