## This function allows me to set the value of the vector with a matrix as
## well as set the function for the inverse, and get the inverse.

## The makeCacheMatrix will return the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(solve(matrix)) inv <<- Inverse
	getInverse <- function() inv
	list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function will allow me to see if the inverse for this matrix has
## already been calculated, and if it has not, assign it a new value
## in the cache.

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setInverse(inv)
	inv
}
