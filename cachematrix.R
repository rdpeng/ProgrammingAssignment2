## Whenever you put new matrix into makeCacheMatrix, the cache will be reset
## and then, if you ran cacheSolve(), you will get the inverse of that matrix.
## if you ran cacheSolve() again before putting another matrix 
## into makeCacheMatrix,you will get cached inverse matrix.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	Invertible_matrix <<- x
	Inverse_cache <<- NULL
}


## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x) {
	if(!is.null(Inverse_cache)) {
		message("getting cached data")
      	return(Inverse_cache)
	}
 	Inverse_cache <<- solve(Invertible_matrix)
	return(Inverse_cache)
}

