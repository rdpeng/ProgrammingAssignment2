## Coursera R programming course - Programming Assignment 2: Lexical Scoping
## Assignment: Caching the Inverse of a Matrix

## This function creates a (special "matrix") object that is a list of functions
## 1. checkdim: trims the function argument (data matrix) to a square matrix
## 2. get: returns the data matrix, i.e. the argument of the (main) function
## 3. set: resets the data matrix to its input/argument matrix
## 4. getinv: returns the inverse matrix if cached, otherwise a NA matrix
## 5. setinv: caches/sets inverse matrix as the matrix given by its argument
makeCacheMatrix <- function(x = matrix()) {

	## 'checkdim' function checks if x is a square matrix. if not 
	## it trims extra rows or columns and returns a square matrix
	checkdim <- function() {
		dims<-dim(x)	# get dimensions of the input matrix
		if (dims[1] != dims[2]) {
			message("Warning: the given matrix is not square,")
			xdim <- min(dims[1], dims[2])	# get the smallest dimension 
			message("turning to a square ", xdim, "x", xdim, " matrix...")
			x <<- x[1:xdim, 1:xdim]
		} else { 
			xdim <- dims[1] 
		}
		xdim	# returns the dimension of the square matrix
	}

	## create an empty square matrix where to store the inverse of x
	xdim <- checkdim()
	invX <- matrix(nrow=xdim, ncol=xdim) 

	## 'get' function returns the current data (matrix x)
	get <- function() x

	## 'set' function resets (values of) the matrix x
	set <- function (Y) {
		x <<- Y
		xdim <- checkdim()
		invX <- matrix(nrow=xdim, ncol=xdim)
	}

	## 'getinv' function returns the inverse matrix of x (if computed)
	getinv <- function() invX

	## 'setinv' function stores the computed inverse of x to invX
	setinv <- function(X1) invX <<- X1

	list(checkdim = checkdim, get = get, set = set,
		getinv = getinv, cacheinv = setinv)
}


## This function computes the inverse matrix of the data matrix passed through
## its argument (the list created by the function above). It first checks if
## inverse is already cashed and returns that value, otherwise computes 
## the inverse and sends it for caching.
cacheSolve <- function(x, ...) {
	X1 <- x$getinv()	# get the current (cached or empty) inverse matrix
	xdim <- x$checkdim()	# get dimension of squared x matrix
	## trim x to squared matrix and assign to data
	data <-x$get()[1:xdim, 1:xdim]

	## Return chached matrix X1 if it is not empty (not any NA value in X1) 
	## and data matrix has not changed after the computation of the inverse X1.
	## 2nd condition is true if X1 * data = identity matrix, diag(xdim).
	## First check that X1 dimension is the same with (x) xdim, then round up 
	## matrix multiplication to remove errors from machine number calculations.
	if (!anyNA(X1) && dim(X1)[1] == xdim && 
		identical(round(X1 %*% data, 4), diag(xdim))) {
		message("Returning the cached inverse matrix.")
		return(X1)
	}

	## Check that determinant of data matrix is not 0 otherwise inverse cannot
	## be computed, find inverse matrix with solve and put it in cache.
	if (det(data) != 0) {
		X1 <- solve(data, ...)
		x$cacheinv(X1)
		return(X1)
	} else {	# matrix determinant is 0 
		message("Matrix determinant is 0, cannot compute the inverse.")
	}
}
