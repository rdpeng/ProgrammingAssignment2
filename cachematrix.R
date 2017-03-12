R_programmingAssignment2


## my solutions##
## The following function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {inv_matrix <- NULL
	set <- function(y) {
		x <<- y
		inv_matrix <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv_matrix <<- inverse
	getInverse <- function() inv_matrix 
	list(set = set,
	get = get,
	setInverse = setInverse,
	getInverse = getInverse)
	}
	
## The following function computes the inverse of the special 
## "matrix" created by makeCacheMatrix above. It should retrieve
##the inverse from the cache when the inverse matrix was ##calculated.

cacheSolve <- function(x, ...) {
	inv_matrix <- x$getInverse()
	if (!is.null(inv_matrix)) {
		message ("getting cached data")
		return (inv_matrix)
		
	}
	matrix <- x$get()
	inv_matrix <- solve(matrix,...)
	x$setInverse(inv_matrix)
	inv_matrix
	}
	
	
