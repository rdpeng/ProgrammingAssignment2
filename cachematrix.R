# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. matrix$set()-set the value of the matrix
# 2. matrix$get()-get the value of the matrix
# 3. matrix$setInverse()-set the value of inverse of the matrix
# 4. matrix$getInverse()-get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
x <<- y
inv <<- NULL
}
get <- function() x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv
list(set = set,
get = get,
setInverse = setInverse,
getInverse = getInverse)
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If yes, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cacheSolve via
# setinverse function.

# This function assumes that the matrix is always invertible
cacheSolve <- function(x, ...) {
inv <- x$getInverse()
if (!is.null(inv)) {
 message("getting cached data")
return(inv)
}
mat <- x$get()
inv <- solve(mat, ...)
x$setInverse(inv)
inv
}
