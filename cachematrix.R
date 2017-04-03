
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        set <- funtion(y) {
                x <<- y
                inv <<- NULL
}
 get <- function() x
         setInverse <- function(sMatrix) inv <<- sMatrix
         getInverse <- function() inv
                 list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
                }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
