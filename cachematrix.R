## makeCacheMatrix is a function that returns the list of functions
## Its purpose is to store martix and cached value of a inverse of matrix
## cacheSolve function calculates the inverse of a special matrix created with makeCacheMatrix

## makeCacheMatrix function

makeCacheMatrix <- function(x = numeric())
{

		# Setting cache value to NULL
        cache <- NULL
		
		# Storing the matrix
        setMatrix <- function(newval)
		{
                x <<- newval
				
				# Flushing the cache
                cache <<- NULL
        }
		
		# Returning the stored matrix
        getMatrix <- function()
		{
                x
        }
		
		# Caching the given argument
        cacheInverse <- function(solve)
		{
                cache <<- solve
        }
		
		# Getting the cached value
        getInverse <- function()
		{
                cache
        }
		
		# Returning a list.
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


## cacheSolve function

cacheSolve <- function(y, ...)
{

		# Getting the cached value
        inverse <- y$getInverse()
		
		# Returning it, if a cached value exists
        if(!is.null(inverse))
		{
                message("Cached data")
                return(inverse)
        }
		
		# Else get the matrix, caclulate the inverse and store it in cache
        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacheInverse(inverse)
		
		# Returning the inverse
        inverse
}
