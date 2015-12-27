## These two functions are meant to be used together; to avoid computing
## several times the inverse of the same matrix.

## This function creates an object, which stores a matrix and its inverse
## (only in case it has already been computed).


makeCacheMatrix <- function(x = matrix()) {
	## 'x' should be an invertible matrix.
	
	inverse <- NULL
		set <- function(y) {
			x <<- y
			inverse <<- NULL
		}
	get <- function() x
	setInverse <- function(solve) inverse <<- solve
	getInverse <- function() inverse
	list(set=set, get=get, setInverse=setInverse, getInverse = getInverse)
}


## This functions retrieves the cached inverse.
## If the matrix has been modified, then it computes the inverse again.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'.
	## 'x' should be of type 'makeCacheMatrix'.
	
	inverse <- x$getInverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data, ...)
	x$setInverse(inverse)
	inverse
}
