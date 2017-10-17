## Assignment : caching the Inverse of a matrix:
## Pair of functions that cache the inverse of a matrix.

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverseMatrix) inv <<- inverseMatrix
	getInverse <- function() inv
	list(set = set,
	     get = get,
	     setInverse = setInverse
	     getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created above
## If the inverse has already been calculated and the matrix has not changed
## then the cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)

	}
	matrix <- x$get()
	inv <- solve(matrix, ...)
	x$setInverse(inv)
	inv
}

