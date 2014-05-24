#Introduction
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

This Assignment2 for R-Programming and usages of the function makeCacheMatrix,cacheSolve  as 
# Example usage:
	# > a <- matrix(rnorm(12), nrow = 4) // Create a matrix a
	# > y <- makeCacheMatrix(a) // Create own special matrix
	# > y$get() // Return the matrix
	# > cacheSolve(y) // Return the inverse
	# > cacheSolve(y) // Call the 2nd time, so return the cached inverse