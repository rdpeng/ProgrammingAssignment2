## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

	##this function creates a matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	minverse<-NULL
	set<-function(y) {		##set the matrix
		x<<-y
		minverse<<-NULL
	}
	get<-function() {			##get the matrix
		x
	}
	setInverse<-function(inverse) {	##set the inverse
		minverse<<-inverse
	}
	getInverse<-function() {		##get the inverse
		minverse
	}
							##returns the list of methods
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

	##this function computes the inverse of a matrix returned
	##by the previous function

cacheSolve <- function(x, ...) {
	minverse<-x$getInverse()
	if(!is.null(minverse)) {
		message("getting cached data - Matrix Inverse")
		return(minverse)
	}
	data<-x$get()			##determining the inverse
	minverse<-solve(data, ...)
	
	x$setInverse(minverse)

	minverse				## Return a matrix that is the inverse of 'x'
}
