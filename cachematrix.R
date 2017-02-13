## The idea of these functions is to cache the inverse of a matrix

## The first function, creates a special ""matrix".

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL										## initializing variable inv as null, will hold the inverse of the matrix
	set <- function(y) {							## defining the set function so we can assign the value of matrix in a parent environment
		x <<- y					
		inv <<- NULL								## If it's a new matrix, reset inv
	}	
	get <- function() x								## define the get function, returns te value of the matrix as argument from the parent environment
	setinverse <- function(inverse) inv <<- inverse	## uses a anonymous function to assign the value of inv in parent environment
	getinverse <- function() inv					## gets the value of inv from the parent environment when called
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)	## needed to refer to the function using the $ operator
}

## The following function computes the inverse of the "matrix" created by the above function.
## If the inverse was already calculated and the matrix wasn't changed, cacheSolve will retrieve from the cache.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
