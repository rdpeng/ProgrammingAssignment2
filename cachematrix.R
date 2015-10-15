## This .R file defines two functions, makeCacheMatrix and cacheSolve 
## The functions are used to cache the inverse of a matrix
##
## Example:
## >a <- matrix(c(0,2,1,0), ncol = 2)
## >ac <- makeCacheMatrix(a)
## >cacheSolve(ac)
##  Calculating inverse matrix
##      [,1] [,2]
## [1,]    0  0.5
## [2,]    1  0.0
## >cacheSolve(ac)
##  Getting cached inverse matrix
##      [,1] [,2]
## [1,]    0  0.5
## [2,]    1  0.0

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	# Initialize inverse matix to NULL
	inverse_matrix <- NULL

	# Set the matrix and inverse values
	set <- function(y) {
		x <<- y
		inverse_matrix <<- NULL
	}

	# Get the matrix value
	get <- function() x

	# Set the inverse matrix value
	setInverseMatrix <- function(inverse) inverse_matrix <<- inverse

	# Get the inverse matrix value
	getInverseMatrix <- function() inverse_matrix

	# List the methods availables
	list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrives the inverse from the cache; otherwise the inverse is calculated
cacheSolve <- function(x, ...) {
	# Get chached inverse matrix
	inverse_matrix <- x$getInverseMatrix()

	# Is the inverse already calculated?
	if (!is.null(inverse_matrix)) {
		message("Getting cached inverse matrix")
		return(inverse_matrix)
	}

	# Otherwise, get data
	data <- x$get()

	# Calculate inverse matrix
	message("Calculating inverse matrix")
	inverse_matrix <- solve(data, ...)

	# Save inverse matric result
	x$setInverseMatrix(inverse_matrix)

	# Return a matrix that is the inverse of 'x'
	inverse_matrix
}
