## My functions create a special matrix and calculate inverse of it

## Create a special matrix

makeCacheMatrix <- function(x = matrix()) {
		ters <- NULL
		set <- function(y) {
				x <<- y
				ters <<- NULL
		}
		get <- function() x
		setinverse <- function(inverse) ters <<- inverse
		getinverse <- function() ters
		list(set=set, get=get, 
			setinverse=setinverse, 
			getinverse=getinverse)

}


## Calculate inverse of the special matrix
## Get the inverse from the cache if it is calculated before

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		ters <- x$getinverse()
		if(!is.null(ters)) {
			message("getting cached data.")
			return(ters)
		}
		data <- x$get()
		ters <- solve(data)
		x$setinverse(ters)
		ters
}
