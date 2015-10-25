## The following functions allow the user to create a matrix
## , compute for its inverse only once, and store the result
## in the cache to make it retrievable if the inverse of the
## same matrix is needed again. 

## Creates a cache matrix and returns a list of functions
## to set the value of the matrix, get the value of the matrix, 
## set the inverse of the matrix, and get the inverse of 
## the matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Solves for the inverse of the matrix created using the makeCacheMatrix function.
## Skips the computation if the inverse has already been computed and just
## retrieves it when needed 

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if (!is.null(i)){
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
        ## Return a matrix that is the inverse of 'x'
}
