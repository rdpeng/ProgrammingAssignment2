## The 'cacheMatrix.R' script contains a pair of functions that
## can cache the inverse of a matrix by using the '<<-' operator. 
## The inverse has been calculated using the 'solve' function in R. 
## This script is based on the assumption that the matrix supplied 
## is always invertible.

## The first function, 'makeCacheMatrix' creates a special "matrix", 
## which is list containing functions to set the set the value of 
## matrix, get the value of matrix, set the value of the inverse 
## and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) s <<- solve
	getInverse <- function() s
	list(set = set, get = get, setInverse = setInverse , getInverse  = getInverse)

}


## The following function computes the inverse of the special "matrix"
## returned by 'makeCacheMatrix' function above. If the inverse has already
## been calculated, it gets the inverse from the cache and skips the computation. 
## Else, it computes the inverse of the matrix and set the value of inverse 
## in the cache via the 'setInverse' function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	s <- x$getInverse()
	if(!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	data <- x$get()
	s <- solve(data, ...)
	x$setInverse(s)
	s
}
