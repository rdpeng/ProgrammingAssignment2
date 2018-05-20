## This function caches the inverse of a matrix instead
## of computing it repeatedly.

## This function creates a special "matrix" object that
## can cache its inverse.

## x is initialized as an argument for the function whose
## default value is a matrix. We need to assign a default
## value so that cacheSolve can be executed without an error.
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	
	## Assign the input value of 'set' to x in the parent environment
	## so that the value of x can be changed without initializing
	## x again.
	## Assign NULL to m in the parent environment so that any value
	## of the inverse that has previously been calculated in m
	## by cacheSolve can be cleared.
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	## Retrieve x from the parent environment of makeCacheMatrix.
	get <- function() x
	
	## Define the setter for the inverse m. Assign the input
	## argument to the value of m in the parent environment.
	setinverse <- function(solve) m <<- solve
	
	## Define the getter for the inverse m.
	getinverse <- function() m
	
	## Assign all four function as named elements of a list so that
	## they can be recalled in the cacheSolve function.
	list(set = set,
	     get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## `makeCacheMatrix` above. If the inverse has already been calculated
## (and the matrix has not changed), then `cacheSolve` should retrieve
## the inverse from the cache.

## cacheSolve requires its input to be an object of type makeCacheMatrix
## because the $ extract operator won't work otherwise.
cacheSolve <- function(x, ...) {
	
	## Get inverse of the object passed as the argument.
	m <- x$getinverse()
	
	## If it is not null, the cached inverse is returned.
	## m could be NULL since makeCacheMatrix sets m to NULL
	## every time a new matrix is passed as an object.
	if(!is.null(m)) {
		message("Getting cached inverse")
		return(m)
	}
	
	## If it is null, the vector is obtained using x$get(),
	## the inverse is calculated, and it is set using the
	## setinverse() function, and the value of the inverse
	## is returned.
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
