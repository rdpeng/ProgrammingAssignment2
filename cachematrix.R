## This set of functions returns the inverse of a matrix.
## If the inverse has already been calculated, the result is returned from the cache,
## else, it is computed and stored in the cache.

## This function, creates a special "vector", which is a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## This function checks if the inverse is available in the cache.
## If the inverse is available in the cache, the result is returned from the cache.
## Else, the result is calculated and returned after stored the result in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(x)
	x$setinverse(m)
	m
}
