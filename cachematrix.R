## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	mat <- NULL
	set <- function(y) {
			x <<- y
			mat <<- NULL
		}
	get <- function() x
	setinverse <- function(solve) mat <<- solve
	getinverse <- function() mat
	list(set = set, get = get,
			setinverse = setinverse,
			getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        		mat <- x$getinverse()
		if(!is.null(mat)) {
			message("Getting Cached Data")
			return(mat)
		}
	data <- x$get()
	mat <- solve(data, ...)
	x$setinverse(mat)
	mat
}
