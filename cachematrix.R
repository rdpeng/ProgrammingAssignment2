## The following functions can be used to avoid doing time-consuming
## calculations every time you want to run your code. They cache the value
## of the inverse of a matrix given after calculating it and retrieve the
## value once you give the same matrix as an argument again.


## This function caches the matrix and its inverse, making a list in order
## to make it easier to retrieve the data 

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}

	get <- function()x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## This function verifies if the inverse matrix wanted was already calculated.
## If so, it will retrieve that value instead of calculating again.
## If not, it will calculate the inverse of the matrix given.

cacheSolve <- function(x, ...) {
	
	m <- x$getinverse()
	if(!is.null(m)){
		message("Getting cached data")
		return(m)
	}

	data <- x$get()
	m <- solve(data,...)
	x$setinverse(m)
	m
}
