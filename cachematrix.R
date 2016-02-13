## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# this function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() {
		x
	}
	setIMatrix <- function(iMatrix) {
		m <<- iMatrix
	}
	getIMatrix <- function() {
		m
	}
	list(set = set, get = get, setIMatrix = setIMatrix, getIMatrix = getIMatrix)
}


## Write a short comment describing this function

# this function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# if the inverse has already been calculated (and the matrix has bit changed), then the cachesolve retrueve the inverse from the cache.
cacheSolve <- function(x, ...) {
	m <- x$getIMatrix()
	if(!is.null(m)) {
		message('getting cached data')
		return(m)
	}
	m <- solve(x$get(), ...)
	x$setIMatrix(m)
	## Return a matrix that is the inverse of 'x'
	m
}
