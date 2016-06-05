##      06-05-2016
##      Kees Wesdorp

######  These functions, used together, will calculate the inverse matrix.
##      If the inverse matrix is already calculated, the functions will return 
##      the cached inverse matrix instead of recalculating the matrix.
##      NOTE: These functions only work on square invertible matrices!

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_invmatrix <- function(solve) m <<- solve
        get_invmatrix <- function() m
        list(set = set, get = get,
             set_invmatrix = set_invmatrix,
             get_invmatrix = get_invmatrix)

}


## this function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$get_invmatrix()
        if(!is.null(m)) {
        message("getting inverse matrix from cache")
        return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_invmatrix(m)
        m
        
}
