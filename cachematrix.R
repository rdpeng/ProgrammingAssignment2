## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inv <- null
	set <- function(y) {
		x <<- y
		inv <<- null
	}
	get <- function() x
	setinv <- function(inv) inv<<- inverse
	getinv <- function() inv
	list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)){
		message ("getting cached data")
		return(inv)
	}
	data <- x$get()
	m<- inv (data, ...)
	x$setinv(inv)
	return(inv)
}
