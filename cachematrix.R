

## makeCacheMatrix makes a matrix which is a list of functions to
## setMat >> sets data into matrix
## getMat >> returns the matrix
## setInv >> sets inverse value in cache
## getInv >> returns inverse from cache

makeCacheMatrix <- function(x = matrix()) {
	inv_cache <- NULL			# holds null till a vale is cached
	setMat <- function(y) {
		x <<- y			# set new value to matrix
		inv_cache <- NULL 	# since the value of matrix has changed, cache = NULL
	}
	getMat <- function () { 
		x				# return the matrix
	}
	setInv <- function(inv) {
		inv_cache <<- inv		# set new inverse in cache
	}
	getInv <- function() {
		inv_cache			# return cached matrix
	}
	list( 				# return a list of functions
		setMat = setMat,
		getMat = getMat,
		setInv = setInv,
		getInv = getInv
		)
}


## This function returns the inverse of matrix.
## If inverse is already calculated, then cached value is rerturned
## If not cached, it calculates the inverse and sets it in cache

cacheSolve <- function(x, ...) {
	inverse <- x$getMat()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	mat <- x&getMat()
	inverse <- solve(mat, ...)
	x&setMat(inverse)
	inverse
}

