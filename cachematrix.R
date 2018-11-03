## Caching the Inverse of a Matrix
## Functions:  makeCacheMatrix and makeCacheMatrix

## makeCacheMatrix:
## creates a special matix, which is really a list containing functions to:
##    set and get de matrix: set, get
##    set and get de inverse of the matrix: setinverse, getinverse


makeCacheMatrix <- function(x = matrix()) {
	r <- NULL
	set <- function(y) {
		x <<- y
		r <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) r <<- solve
	getinverse <- function() r
	list (set= set, get= get, 
		setinverse = setinverse,
		getinverse = getinverse)
}


## makeCacheMatrix:
## solves the matrix using, if posible, the inverse cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	r <- x$getinverse()
        if(!is.null(r)) {
                message("getting cached data")
                return(r)
        }
        data <- x$get()
        r <- solve(data, ...)
        x$setinverse(r)
        r
}
