## Two functions: [makeCacheMatrix] and [cacheSolve]

## [makeCacheMatrix]: 
##		creates a "special matrix" object that can cache its inverse.
##		makeCacheMatrix "Methods": 
##			set(): sets new matrix data and wipes cached inverse
##			get(): returns matrix data
##			set_inverse(): caches (hopefully) calculated inverse
##			get_inverse(): retrieves cached inverse calculation
##		"x" and inverse "i" are essentially private data

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y){
		x <<- y
		i <<- NULL
	}
	get <- function(){ x }
	set_inverse <- function(x_inv){ i <<- x_inv}
	get_inverse <- function(){ i }
	
	list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## [cacheSolve]: 
##		computes the inverse of the "special matrix" object if it hasn't already
##		been computed, or retrieves the inverse from the cache if it has been
##		computed before and the "special matrix" has not changed.

cacheSolve <- function(x, ...) {
	i <- x$get_inverse()
	
	if(is.null(i)) {
		my_matrix <- x$get()
		i <- solve(my_matrix)
		x$set_inverse(i)
	} else {
		message("getting cached data")
	}
	
	i
}
