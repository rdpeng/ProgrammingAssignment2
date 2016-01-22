## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This first function "makeCacheMatrix" is similar to an object constructor
# statement in that it will provide an "getter" and "setter" for the object that
# is created.


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
	set <- function(y) {
	        x <<- y
		i <<- NULL
	}
	get <- function() x
	seti <- function(solve) i <<- solve
	geti <- function() i
	list(set = set, get = get, 
	        seti = seti,
		geti = geti)
}


## Write a short comment describing this function

# This function will detemrine if a solution to the matrix inversion exist and if
# it does that soluiton will be returned. Instead, if a solutio nhas not been
# reached then it will compute the inversion and return the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$geti()
	if(!is.null(i)) {
	        message("getting matrix inversion cache")
		return(i)
	}
	mi <- x$get()
	i <- solve(data, ...)
	x$seti(i)
	i
}
