## Caching the Inverse of a Matrix
## The purpose of this function is to cache the inverse of a matrix
## This will potentially avoid repeated calculations and thereby save time 

## makeCacheMatrix creates a special matrix which
## sets and acquires the value of the matrix and its inverse

## in order to run this code cacheSolve must be called on makeCacheMatrix
## cacheSolve(makeCacheMatrix(matrix))

makeCacheMatrix <- function(x = matrix()) {
	##follows the same setup as the makeVector function from the example
	##replacing m with invert
	invert = NULL
	set = function(y) {
			x <<- y
			invert <<- NULL
	}
	get = function() x
	setinvert = function(inverse) invert <<- inverse 
	getinvert = function() invert
	list(set=set, get=get, setinvert=setinvert, getinvert=getinvert)
}

## cacheSolve calculates the inverse of the special matrix created above
## First it checks to see if the inverse has already been calculated
## If so it will grab the calculated value and skip the computation
## Otherwise it will calculate the inverse

cacheSolve <- function(x, ...) {
	invert = x$getinvert()
	## if the invert value is not null, and has been calculated, grab from cache
	if (!is.null(invert)){
			message("getting cached data")
			return(invert)
	}
	## if the value is null, it will skip the if and calculate the inverse
	##mat.data used in place of data
	mat.data = x$get()
	invert = solve(mat.data, ...)
	x$setinv(invert)
	## using return to explicitly return the invert variable
	return(invert)
}
