## This pair of functions caches the inverse of a matrix


## Function 1
## Function that creates a matrix object that can cache the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
		
}


## Function 2
## Function returns a matrix that is the inverse of 'x'
## First, it looks for cached data and returns the cached inverse of the matrix
## If there is no cached data, it calculates the inverse using the function solve()

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
    if(!is.null(m)) {
    	message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
