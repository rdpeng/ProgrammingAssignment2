## Put comments here that give an overall description of what your
## functions do

## makeCache creates a special “matrix” containing a function to set and get the value of the matrix
## and set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
	i <- NULL
	set <- function(y) 
	{
		x <<- y
		i <<- NULL
	}
	get <- function() x
	set_inv <- function(inv) i <<- inv
	get_inv <- function() i
	list(set = set,get = get,set_inv = set_inv,get_inv = get_inv)
}


## The following function calculates the inverse of the matrix created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value 
## of the inverse in the cache via the solve function.

cacheSolve <- function(x, ...) 
{
	i <- x$get_inv()
	if (!is.null(i)) 
	{
		message("getting cached data")
		return(i)
	}
	m <- x$get()
	i <- solve(m, ...)
	x$set_inv(i)
        i
}
