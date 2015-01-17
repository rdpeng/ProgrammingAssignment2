makeCacheMatrix <- function(x){

	# This function creates a special "matrix" 
	# object that can cache its inverse.
	
	i <- NULL

	set <- function(y) {
	    # set the value of the vector
	    x <<- y
	    m <<- NULL
	}
	
	get <- function(){
	    # get the value of the vector
	    x
	}
	
	setInverse <- function(inverse){
	    # set the value of the mean
	    i <<- inverse
	}
	
	getInverse <- function(){
	    # get the value of the mean
	    i
	}
	
	list (set = set, get = get, setInverse = setInverse, getmean = getInverse

}

cacheSolve <- function(x){

	# This function computes the inverse of the special 
	# "matrix" returned by makeCacheMatrix above. If the 
	# inverse has already been calculated (and the matrix 
	# has not changed), then cacheSolve should retrieve the 
	# inverse from the cache.
	
	i <- x$getInverse()
	
	if(!is.null(i)) {
	    message("getting cached data")
	    return(i)
	}
	
	data <- x$get()
	
	i <- solve(data, ...)
	
	x$setInverse(i)
	
	i


}
