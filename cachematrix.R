## Put comments here that give an overall description of what your
## functions do

## This function stores computations to cache user-input matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL

## This function sets the value of the matrix
 	setMatrix <- function(y) {
  	x <<- y
 	i <<- NULL
  }

## This function obtains the value of the matrix that was set by the previous function
 	getMatrix <- function() 
 	x

## This function sets the inputted matrix into an inverse matrix
 	setInverseMatrix <- function(inverse) 
 	i <<- inverse

## This function obtains the inverse matrix
	getInverseMatrix <- function() 
 	i

## The list function contains both the inpputed and inverse matrix 
 	list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## This function is responsible in solving the matrix

cacheSolve <- function(x, ...) {
	i <- x$getInverseMatrix()
 	if (!is.null(i)) {
 	message("Extracting Cached Matrix") ## This message will appear if the data is already existing in the cache
        ## Return a matrix that is the inverse of 'x'
	return(i)
  }
 	m <- x$getMatrix()
 	i <- solve(m, ...)
	x$setInverseMatrix(i)
 	i
}

