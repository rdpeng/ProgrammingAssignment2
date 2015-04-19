## These sets of functions can be uset to cache the inverse of a matrix in order to avoid computing it repeatedly

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## variable invmat will store the inverse of the matrix
        invmat <- NULL
        set <- function(y) {
                x <<- y
                invmat <<- NULL
        }
        get <- function() x
        setinv <- function(inv) invmat <<- inv
        getinv <- function() invmat
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invmat <- x$getinv()
        if(!is.null(invmat)) {
                message("getting cached inverse matrix")
                return(invmat)
        }
        data <- x$get()
        invmat <- solve(data, ...)
        x$setinv(invmat)
        invmat
}
