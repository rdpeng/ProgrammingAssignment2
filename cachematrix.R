## function makeCacheMatrix is function that build a container of 
##	Two values: matrix and its solved result
##	Four helper functions that get/set the matrix and its solve result
## Return value: a list that contain the values and fuctions

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	sol <-NULL
	set <- function (x) {
		mat <<- x
		sol <<- NULL
	}
	get <- function () sol

	setSolve <- function (x) sol <<- x
	getSolve <- function () sol
	list(set = set, get=get,
		setsolve = setsolve,
		getsolve = getsolve)
	
}


## function cacheSolve solve the matrix created in object x. 
## It caches the result in object x when object x is first solved.
## the cached result is used directly without calculation again in later call.
## 
## return value: the result of the matrix solve().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	result <- x$getSolve()
	if (!is.null(result)) {
		message ("Cache hitted!")
		return (result)
	}
	result <- solve(x$get, ...)
	x$setSolve(result)
	result

}
