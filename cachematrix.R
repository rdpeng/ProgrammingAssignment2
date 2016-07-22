## The combination of these two functions serves the purpose of caching the inverse of a matrix.

## The makeCacheMatrix build a specific matrix object that can cache its inverse.

makeCacheMatrix <- function(special = matrix()) 
{
	i <- NULL
	set <- function(temp)
	{
		special <<- temp
		inv <<- NULL
	}

	get <- function()
	{
		special
	}

	setInverse <- function(inverse)
	{
		i <<- inverse
	}

	getInverse <- function()
	{
		i
	}
	
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function compiles the inverse of the matrix given by the makeCacheMatrix function.

cacheSolve <- function(special, ...) 
{
	i <- special$getInverse()
	if(!is.null(i))
	{
		message("Getting cached data...")
                return(i)
	}
	matrix <- special$get()
      i <- solve(matrix, ...)
      special$setInverse(i)
      i
}
