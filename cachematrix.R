## Put comments here that give an overall description of what your
## functions do
## Functions to create and manipulate a matrix with cached data.

## Write a short comment describing this function
## Create a matrix, if none is given, and provide functions for data manipulation.

makeCacheMatrix <- function(x = matrix()) {

	I <- NULL
	set <- function(y) {
	
		x <<- y
		I <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) I <<- inverse
	getInverse <- function() I
	
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## Compute the inverse of the 'CachedMatrix' provided and return it.
## If available, return the cached data instead.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		I <- x$getInverse()
		if(!is.null(I)) {
		
			message("getting cached data")
			return(I)
		}
		data <- x$get()
		I <- solve(data, ...)
		x$setInverse(I)
		I
}
