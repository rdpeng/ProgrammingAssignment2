## The makeCache function creates a matrix object that can cache its inverse.
## The value of the matrix is initialized by the argument. 
## After initialization the matrix value can be set using the set function and
## retrieved using the get function.
## The inverse of the matrix can be set using setInv function even though 
## this is not suggested since it can be calculated and cached using cacheSolve
## The inverse of the matrix van be retrieved by getInv function provided
## that it has already been cached

makeCacheMatrix <- function(x = matrix()) {
        invMat <- NULL
	set <- function(y) {
		x <<- y
		invMat <<- NULL
	}
	get <- function() x
	setInv <- function(inverse) invMat <<- inverse
	getInv <- function() invMat
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## The cacheSolve function checks if the inverse of the matrix 
## has already been cached and if so it returns it.
## Otherwise it calculates the inverse of the matrix 
## using solve function and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMat <- x$getInv()
	if (!is.null(invMat)) {
		message ("getting cached data")
		return(invMat)
	}
	data <- x$get()
	invMat <- solve(data, ...)	
	x$setInv(invMat)
	invMat
}
