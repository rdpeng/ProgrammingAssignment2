## makeCacheMatrix and cacheSolve are a pair of functions that aim to cache the inverse of a matrix
 
## makeCacheMatrix is a function that caches the inverse of a matrix object


makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function (y){
		x <<- y
		m <<- NULL
	}
	get <- function () x
	setinverse <- function(solve)m <<-solve
	getinverse <- function()m
	list(set = set, get = get,
	setinverse = setinverse,
	getinverse = getinverse)
}

## The function cacheSolve computes the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x,...){
	m <- x$getinverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	mat <- x$get()
	m <- solve(mat, ...)
	x$setinverse(m)
	m
}