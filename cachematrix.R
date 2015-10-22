makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
	 setinverse = setinverse,
	 getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
			message("getting cached data")
			return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
}

###############
#Example of use
#data<-matrix(sample(1:900,900),nrow=30)

#M<-makeCacheMatrix(data)

# First estimation of the mean
#cacheSolve(M)

# Second estimation of the mean
#cacheSolve(M)
