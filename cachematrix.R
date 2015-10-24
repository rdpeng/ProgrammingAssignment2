## For this week assignment, we will build a function that enables you to cache
## potentially time-consuming computations. This will take advantage of the scoping
## rules of the R language.

# Assignment: Caching the Inverse of a Matrix

# First function, creates a special "matrix" which is really a list
# containing functions to
	# 1. set the value of the matrix
	# 2. get the value of the matrix
	# 3. set the value of the inverse matrix (cached)
	# 4. get the value of the inverse matrix (cached)

makeCacheMatrix <- function(x = matrix()) {
	invMat <- NULL

	# Matrix
	setMat <- function(y) {
		x <<- y
		invMat <<- NULL
	}
	# Return of the matrix
	getMat <- function() x
	
	# We set the cached value 
	setInvMat <- function(solve) invMat <<- solve
	
	# We get the cached value
	getInvMat <- function() invMat
	
	# Return a list of all the functions
	list(setMat = setMat, getMat = getMat, setInvMat = setInvMat,
		getInvMat = getInvMat)
}

# Second function, calculates the inverse of the special "matrix" returned by
# makeCacheMatrix function above. If the inverse has already been calculated
# (and the matrix has not changed), then the cacheSolve should retrieve the
# inverse from the cache.

cacheSolve <- function(x, ...) {
	invMat <- x$getInvMat()
	if(!is.null(invMat)) {
		message("getting cached data")
		return(invMat)
	}
	data <- x$getMat()
	invMat <- solve(data, ...)
	x$setInvMat(invMat)
	invMat
}
