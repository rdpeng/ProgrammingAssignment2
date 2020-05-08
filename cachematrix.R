## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
Checks if cache matrix 

# first checks with cache matrix function if one is present otherwise 
# actual get-set logic function

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) inverse <<- solve
	getInverse <- function() inverse
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
# call and check function

cacheSolve <- function(x, ...) {
	x <- x$getInverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data, ...)
	x$setInverse(inverse)
	inverse
}}
