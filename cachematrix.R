# makeCacheMatrix: This function creates a special "matrix" object that can 
# cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL # create a new variable called i

	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	
	get <- function() x
	setinverse <- function(inverse) i <<- inverse # set the inverse of data
	getinverse <- function() i # get the inverse
	
	list(set = set, get = get, setinverse = setinverse, 
		getinverse = getinverse) # list out the result
}


# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
# Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)) {
                message("getting cached data")
                return(i)
	}
	data <- x$get()
	i <- inverse(data, ...)
	x$setmean(i)
	i
}
