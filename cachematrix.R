## Matrix inversion is costly to compute so we cache the inverse of the matrix.

# This function creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

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


# The following function returns the inverse of the matrix. If the inverse has been computed, it skips the computation and return the result# If not, the function computes the inverse. 



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
