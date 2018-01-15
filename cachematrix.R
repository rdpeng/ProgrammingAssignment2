## coursera : R programing W3 Programming Assignment 2
## Funtion "makeCacheMatrix" 
## This function creates a special "matrix" object that can cache its inverse.
## setting matrix, getting matrix, setting inverse and getting inverse.

makeCacheMatrix <- function(x = numeric()) 
{
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(mean) m <<- mean
        getinv <- function() m
        list(	
				set = set,
				get = get,
				setinv = setinv,
				getinv = getinv
			)
}




## Function "cacheSolve"
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed)
## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) 
{
## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinv(inv)
	inv
}