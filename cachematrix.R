## Put comments here that give an overall description of what your
## functions do
## The following two functions compute the inverse of a matrix and cache respectively.

## The function ("makeCacheMatrix") creates a special "matrix" object that cache its inverse.
	
## This function ("makeCacheMatrix") assumes that the matrix is always invertible.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
	
set <- function(y) {
	
x <<- y
	
inv <<- NULL
	
}
	
get <- function() x
	
setinverse_matrix <- function(inverse) inv <<- inverse
	
getinverse_matrix <- function() inv
	
list(set=set, get=get, setinverse_matrix=setinverse_matrix, getinverse_matrix=getinverse_matrix)
}

## The ("cacheSolve") function  computes the inverse of the matrix
	
## returned by "makeCacheMatrix" function as displayed above.
	
## It checks if the inverse of the matrix has already been computed and then gets the result and skips the computation.
	
## If the inverse of the matrix is not computed, it then computes the inverse of the matrix,
	
## sets the value in the cache via setinverse function.

cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
	
if(!is.null(inv)) {
	
message("getting cached data.")
	
return(inv)
	
}
	
data <- x$get()
	
inv <- solve(data)
	
x$setinverse(inv)
	
inv 
}
