# The following methods help with reducing the costly computation of 
# calculating the inverse of a matrix by storing it in the cache rather
# than recalculating the inverse anytime the inverse needs to be used.

# The makeCacheMatrix creates a list containing methods to do a number
# of operations that pertain to dealing with the matrix on it's inverse.
# - makeCacheMatrix$set - sets the value of the matrix
# - makeCacheMatrix$get - gets the value of the matrix
# - makeCacheMatrix$setInverse - sets the inverse of the matrix
# - makeCacheMatrix$getInverse - gets the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	
	# Sets the value of the matrix
	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	# Gets the value of the matrix
	get <- function() x

	# Sets the value of the inverse in the cache
	setInverse <- function(inverse) inv <<- inverse

	# Gets the value of the inverse that's stored in the cache
	getInverse <- function() inv

	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


# cacheSolve method will return the inverse of the matrix. When it's executed
# it first checks to see if the inverse has already been computed and cached.
# If it has, it returns the cached inverse. If it hasn't, it then computes 
# the inverse, and stores it in the cache with the setInverse method.

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()

	if(!is.null(inv)) {
		# Returns the cahced inverse
		return(inv)
	} else {
		# Get the matrix, calculates the inverse, caches the result and then returns the inverse
		matrix <- x$get()
		inv <- solve(matrix)
		x$setInverse(inv)
		return(inv)
	}
}



