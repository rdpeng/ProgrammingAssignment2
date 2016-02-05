## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Variables:
## 			- inv : inverse of matrix
##			- x : matrix
## 			- y : copy of matrix x
##			- set : function to set y to x
##			- get : function to return matrix x
##			- setInverse : sets the inverse of matrix 
##			- getInverse : returns the inverse
##			

makeCacheMatrix <- function(x = matrix()) {


	## set null value placeholder for inverse
	inv <- NULL
	
	
	## define a function to set the matrix x to a new matrix y
	## and reset the inverse
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	
	## "get" return matrix x
	get <- function() x
	
	
	## setInverse sets the inverse of the matrix
	setInverse <- function(inver) inv <<- inver
	
	
	## getInverse returns the inverse
	getInverse <- function() inv
	
	
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
	
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		inv <- x$getInverse()
		if (!is.null(inv)) {
			message("getting cached data")
			return(inv)
		}
		data <- x$get()
		inv <- solve(data, ...)
		x$setInverse(inv)
		inv
}
