## These functions written in fulfillment of the
## Week 3 Assignment; August 2019; GitHub user: zac-christin

## The following function creates a matrix object that can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL  
	set <- function(y) { 
		x <<- y
		inv <<- NULL
	}
	get <- function() x
##
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}


## The following function computes the inverse of the matrix returned by the function above.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
