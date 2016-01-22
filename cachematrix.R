## The two functions below are used to cache the inverse of a matrix
## It is assummed that the matrix supplied is always invertible.
##
## The operator "<<-" is introduced in this exercise to show how to
## assign a value to an object in an environment that is different
## from the current environment.

## Function "makeCacheMatrix" 

   ## Creates a "special" matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
	}
	get <- function() x
	setinv <- function(solve) m <<- solve
	getinv <- function () m
	list(set = set, get = get,
	     setinv = setinv,
	     getinv = getinv)

}


## Function "cacheSolve"

   ## Return a matrix that is the inverse of 'x'
   ## If the inverse has already been calculated and the matrix has not changed,
   ## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

	m <- x$getinv()
	if(!is.null(m)){
	        message("getting cached inverse")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinv(m)
	m
}
