## Caching the Inverse of a Matrix
## Put comments here that give an overall description of what your
## functions do

## Basically creating the function and setting its parameters is the first step, followed by declaring the variables x, y needed in order to solve for the inverse
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function (y) {
	x <<- y
	inv <<- NULL
}
get <- function()x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function () inv
list (set = set, get = get,
setInverse = setInverse,
getInverse = getInverse)

}

##The following is a Solve function that we're going to use in order for the inverse of x to be returned 
cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <-  x$get()
	inv <- solve(data, ...)
	x$setInverse(inv)
	return(inv)
        ## Return a matrix that is the inverse of 'x'
}

