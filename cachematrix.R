## Create an artificial matrix, that contains room for both itself and its
## inverse. 

makeCacheMatrix <- function(x = matrix()) {
	
	## First equate the inverse to NULL, it might not be requested at all since
	minv <- NULL
	
	## Provide a Setter
	set <- function(y) {
		x <<- y
		minv <<- NULL
	}
	
	## Provide a Getter
	get <- function() x
	
	## Provide a Setter for the Inverse
	setinv <- function(inv) minv <<- inv
	
	## Provide a Getter for the Inverse
	getinv <- function() minv
	
	## Return the Object with all of its functionalities
	list(set = set, get = get,
	     setinv = setinv,
	     getinv = getinv)
}


## Returns given matrix's inverse. If the inverse is requested before, then
## will be provided from the cache, if the inverse is not there, it will be
## calculated on-line and saved to cache 

cacheSolve <- function(x, ...) {
	## Get the inverse (might be NULL)
	minv <- x$getinv()
	
	## If it's NULL, re-calculate it
	if(!is.null(minv)) {
		## It's not NULL, we calculated it before, return the cached value
		message("getting cached data")
		return(minv)
	}
	
	## Calculate the Inverse
	data <- x$get()
	minv <- solve(data, ...)
	x$setinv(minv)
	minv
}
