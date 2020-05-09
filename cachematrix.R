## Put comments here that give an overall description of what your
## functions do

## function to create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function (y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(solve) inv <<- solve
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv =  getinv)
}


## computes the inverse of the special "matrix" returned by makeCache Matrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
        	message("getting cached data")
        	return(inv)
        }
        invmatr <- x$get()
        inv <- solve(invmatr,...)
        x$setinv(inv)
        inv
}
