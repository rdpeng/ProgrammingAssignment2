## These functions compute the inverse of a matrix, and if there 
## exists a cache for the inverse of this matrix, 
## return the cache directly.

## Return a list and store the inverse of `x`
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		i <<- NULL
	}
	get <- function() x

	# compute the inverse of a matrix
	setinverse <- function(solve) i <<- solve

	# store the inverse of a matrix
	getinverse <- function() i
	list(
		set = set, 
		get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Return a matrix that is the inverse of `x`
cacheSolve <- function(x, ...) {
	
	## check the cache
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	
	## if not exist cache, rerun `solve`
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)

	## return the inverse of `x`
	i
}
