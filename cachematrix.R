## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Function makes a matrix to cache it's inverse
## Start by having inv as NULL to have value of matrix inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL ##With new matrix, reset inv to NULL
	}
	get <- function() [x] ## Return of matrix argument through get function
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() {inv}
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
}


## Write a short comment describing this function

## Needed to use the makeCacheMatrix function up there
## CacheSolve used to get the inverse if there's an inverse
## Return the inverse of x
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solv(data, ...)
	x$getInverse(inv)
	inv
        ## Return a matrix that is the inverse of 'x'
}
