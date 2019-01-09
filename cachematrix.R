## The following functions retrieve the inverse of a matrix.
## However, if the inverse of a matrix was already calculated before,
## the cached solution instead will be shown.

## This function creates the matrix and sets up the caching mechanism.

makeCacheMatrix <- function(x = matrix()) {
	##Create the matrix that can cache its inverse.
	v <- NULL				
	set <- function(y) {
		x <<- y
		v <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) v <<- solve
	getInverse <- function() v
	list(set = set, get = get, 
			setInverse = setInverse, 
			getInverse = getInverse)
}


## This function returns the inverse of a matrix if not calculated beforehand; 
## otherwise, it returns the inverse of a matrix from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	v <- x$getInverse()
	if(!is.null(v)) {
		message("getting cached data")
		return(v)
	}
	data <- x$get()
	v <- solve(data, ...)
	x$setInverse(v)
	v
}


## Sample matrix
w <- matrix(runif(16, 1, 10), 4, 4)
z <- makeCacheMatrix(w)
cacheSolve(z)

## Confirm the inverse of matrix w is cached
cacheSolve(z)
