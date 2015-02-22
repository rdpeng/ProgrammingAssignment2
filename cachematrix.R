## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	get <- function() x

	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv

	list(set=set, 
			 get=get, 
			 setInverse=setInverse, 
			 getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if(!is.null(inv)) {
		message("ISH IS CACHED")
		return(inv)
	} else {
		data <- x$get()
		inv <- solve(data)
		x$setInverse(inv)
		return(inv)
	}
}



