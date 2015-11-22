## This function takes an invertible matrix to return a vector that 
## contains functions to 1) set the value of vector, 2}get the value, 
## 3)set the inverse of the matrix, and 4)get the inverse. 

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function (z) {
		x <<- z
		inverse <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) inverse <<- solve
	getInverse <- function() inverse
	list(set = set, get = get, 
	setInverse = setInverse, getInverse = getInverse)
}


## This function returns the inverse of the vector created with makeCacheMtrix 
## by first getting it from cache if available. 

cacheSolve <- function(x, ...) {
	inverse <- x$getInverse()
	if(!is.null(inverse)){
		message("getting cached data")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data, ...)
	x$setInverse(inverse)
    inverse ## Return a matrix that is the inverse of 'x'
}
