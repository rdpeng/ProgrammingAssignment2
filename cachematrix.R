makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x<<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set,
		  get = get,
		  setinverse = setinverse,
		  getinverse = getinverse)
}


## cacheSolve function can computes a matrix's inverse of the matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m)) {
        	message("getting cached data")
        	return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
