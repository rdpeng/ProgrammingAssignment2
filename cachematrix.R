## This function `makeCacheMatrix` creates a list containing
## functions to set and retrieve the vaue of a matrix and
## to set and retrieve the value of its inverse
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Retrieve the cached inverse of the matrix or calculate it.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
        
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
