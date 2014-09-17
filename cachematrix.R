## Put comments here that give an overall description of what your
## functions do
## These functions will make caching the inverse of a matrix possible after a first calculation, 
##      rather than compute it every time one needs it.

## Write a short comment describing this function
## This function creates a "matrix" object that can cache its inverse, 
##	through creating a list of funtions.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	## Setting the matrix
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	## Getting the matrix back
	get <- function() {
		x
	}
	## Set the inverse of the matrix
	setinverse <- function(inverse) {
		m <<- inverse
	}
	## Retrieve the inverse of the matrix
	getinverse <- function() {
		m
	}
	## Setting the right names to the functions
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
## This function computes the inverse of the "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
##	then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                m <- x$getinverse()
        ## When the inverse has already been calculated
        ## the inverse is retrieved and printed
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Get the matrix
        data <- x$get()
        ## Calculate the inverse of the matrix
        m <- solve(data, ...)
        ## Caching the inverse for later use
        x$setinverse(m)
        ## Print the calculated inverse
        m
}
