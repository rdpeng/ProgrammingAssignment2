# The following functions will cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function that
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        a <- NULL
        set <- function(y) {
                x <<- y
                a <<- NULL
        }
        get <- function() x
        setinv <- function(inv) a <<- inv
        getinv <- function() a
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# The next function returns the inverse of the matrix. However, it first 
# checks if the inverse has already been computed. If so, it gets the result 
# and skips the computation. Otherwise, it computes the inverse, sets the value 
# in the cache via setinverse function.

## For this assignment, assume that the matrix supplied is always invertible.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        a <- x$getinv()
        if(!is.null(a)) {
                message("getting cached data")
                return(a)
        }
        data <- x$get()
        a <- solve(data, ...)
        x$setinv(a)
        i
}



