## Using lexical scoping, this function creates a pair, x and inverse. X is a matrix
and inverse is the inverse of that matrix. It also defines four functions you can use:
set, get, setinverse, and getinverse. 

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) inverse <<- inv
	getinverse <- function() inverse
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function will check for the inverse of the matrix in the environment and return 
it if it is found. Otherwise, it will calculate the inverse of the matrix and cache it. 

cacheSolve <- function(x, ...) {
	inverse <- x$getinverse()
	if(is.null(inverse)) {
		message("inverse not found")
		value <- x$get()
		inverse <- solve(value, ...)
		x$setinverse(inverse)
		return(inverse)
	}
	
		message("inverse found")
		inverse

	
}
