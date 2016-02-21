## this function creates a special "object" matrix that can 
## cache the inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) {
        cinv <- NULL
        set <- function(y) {
                x <<- y
                cinv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) cinv <<- inverse
        getinv<- function() cinv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## this function computes the inverse of the matrix object returned by
## the function above. If the inverse has already been calcuated,
## and the matrix has not changed, then this function should use 
## the cached inverse 

cacheSolve <- function(x, ...) {
        cinv <- x$getinv()
        if(!is.null(cinv)) {
                message("getting cached data")
                return(cinv)
        }
        data <- x$get()
        cinv <- solve(data, ...)
        x$setinv(cinv)
        cinv
}






