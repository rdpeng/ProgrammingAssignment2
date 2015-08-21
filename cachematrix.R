## Code by Leonardo Maciel
## Date 08/20/2015
## Function cacheSolve returns the inverse of a matrix 
## and stores it if hasn't been calculated before
## First expand your regular matrix using the makeCacheMatrix function on it

## This function expands matrix to support caches  
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, 
		 get = get,
		 setinv = setinv,
		 getinv = getinv)
}


## casheSolve returns the inverse of a matrix if cashed or calculates the inverse
cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	## if inverse matrix exists returns it
	if(!is.null(inv)) {
		## message("found inverse in cache")
		return(inv)
	}
	## calculates inverse and stores it
	mat <- x$get()
	inv <- solve(mat)
	x$setinv(inv)
	return(inv)
}
