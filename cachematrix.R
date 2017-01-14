## This function caches the inverse of a square matrix. Matrix inversion is typically a strenuous computation; caching the inverse can be easier than repeatedly computing the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
## This function inputs a list of functions 1) set the matrix 2) get the matrix 3) set the inverse and 4) get the inverse, to cache the inverse
inv <- NULL
set <- function (y) {
	##'<<-' is used to assign a value to an object that is in a different evironment than the current one 
	x <<- y
	inv <<- NULL 
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set=set, get=get, setinv=setinv, getinverse=getinv)
}

## This function returns the inverse of a square, invertible matrix. 
cacheSolve <- function(x, ...) {
        ## Returns the inverse of matrix 'x'
        inv <- x$getinv()
        ## If the inverse was already calculated
        if(!is.null(inv)){
        	message("getting cached data")
        	return(inv)
        	}
        ## If not, calculates the inverse
        c.data <- x$get()
        inv <- solve(c.data, ...)
        x$setinv(inv)
        
        return(inv)
}

## Sample test 
m <- diag(7,4)
m
CachedMatrix <- makeCacheMatrix(m)
cacheSolve(CachedMatrix)
