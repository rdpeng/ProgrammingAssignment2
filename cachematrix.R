## Put comments here that give an overall description of what your
## functions do

## Creates a "special" matrix object that can store its own inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse<-NULL
	set <- function(y){
		x<<-y
		m<<-NULL
	}
	get <- function() x
	setInverse <- function(i) inverse<<-i
	getInverse <- function() inverse
	list(set=set,get=get,
		setInverse=setInverse,
		getInverse=getInverse)
}


## Either fetches the cached inverse of a CacheMatrix object, or calculates it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        	inverse <- x$getInverse()
	if(!is.null(inverse)) {
		message('getting chached data')
		return(inverse)
	}
	data <- x$get()
	inverse <-solve(data)
	x$setInverse(inverse)
	inverse
}
