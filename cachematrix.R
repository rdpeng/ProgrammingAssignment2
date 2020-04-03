## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated then the cachesolve retrieves the inverse from the cache

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

x<-matrix()
makeCacheMatrix<-function(x)
{
	z<- NULL
	set <- function(y)
	{
		x <<- y
		z <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse)
	{
		z<<-inverse
	}
	getinverse <- function()
	{
		z
	}
	list(set = set, get = get, setinverse = setinverse,getinverse = getinverse)
}
## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) 
{
        z <- x$getinverse()
        if (!is.null(z))
        {
                message("getting cached data")
                return(z)
        }
        data <- x$get()
        z <- solve(data, ...)
        x$setinverse(z)
        z
}
