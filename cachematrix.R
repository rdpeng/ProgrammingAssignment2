## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix builds the base infrastructure for holding/returning the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) m <<- inverse
	getInverse <- function() m
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## utilizing the makeCacheMatrix function defined above, cacheSolve solves for the inverse matrix if none is
## cached, otherwise returns the cached value

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
	if(!is.null(m)) {
		message("Retrieving cached data")
		return(m)
	}
	m <- solve(x$get())
	x$setInverse(m)
	m
}
