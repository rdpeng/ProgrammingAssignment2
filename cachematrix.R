##  A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x ##returns x
	setinverse <- function(inv) i <<- inv ##sets i as inv
	getinverse <- function() i ## returns i
	list(set = set, get=get, setinverse= setinverse, getinverse = getinverse)
}


##  This function computes the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)) {
		message("Getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data) %*% data
	x$setinverse(i)
	i
}
