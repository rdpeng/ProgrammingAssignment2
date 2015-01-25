## The following functions use lexical scoping to cache and retrieve the inverse of a matrix 'x'

## The first function obtains a matrix x and defines 4 functions which are returned in a list
## setm - generates and caches a matrix x from an input vector
## getm - retrieves the matrix x
## getinverse - retrieves the inverse of x which is m
## setinverse - sets the inverse of x
## The labels for the functions in the list are set for setm(), gets for getm(), setinv for setinverse(), getinv = getinverse()

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	setm <- function(y) {
		
		sqrt.y <- sqrt(length(y))
		if(sqrt.y == round(sqrt.y))
		{
			m <<- NULL
			x <<- matrix(data = y, nrow= sqrt.y, ncol= sqrt.y, byrow = TRUE)
			print("The matrix is :")
			print(x)
			return(x)
		}
		else
		{
			print("You can find the inverse only for a square matrix")
		}
	}
	
	getm <- function() {
		x
	}
	
	getinverse <- function()
	{
		print("Fetching the inverse...")
		return(m)
	}
	
	setinverse <- function(p)
	{
		m <<- p
		print("Has updated the inverse")
	}
	
	list(set = setm, gets = getm, setinv = setinverse, getinv = getinverse)

}
	

## cacheSolve is a function that obtains the list object initialized over a matrix 'x' using makeCacheMatrix()
## and retrieves the cached value of the inverse of 'x'

cacheSolve <- function(x = list(), ...) {
        ## Return a matrix that is the inverse of 'x'
		## Here the first argument is the list object that contains the methods defined in makeCacheMatrix
		
		m <- x$getinv()
		print(m)
		
		# If the value is already there in the cache, retrieve it or else, compute the inverse and cache it
		if(!is.null(m))
		{
			print("Getting cached inverse value...")
			return(m)
		}
		else
		{
			y <- x$gets()
			print("Matrix has been updated")
			m <- solve(y)
			x$setinv(m)
			m
		}
		
}
