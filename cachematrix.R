## These two functions cache the inverse matrix of a given one. 

## makeCacheMatrix creates a list of functions: to set and get the value of the matrix and to set and get the inverse. 

makeCacheMatrix <- function(x = matrix()) {
	invM <- NULL
	set <- function(y){
		x <<- y
		invM <<- NULL
	}
	get <- function() x
	setInv <- function(inverse) InvM <<- inverse
	getInv <- function() InvM

      list(set = set, 
		get = get, 
		setInv = setInv, 
		getInv = getInv)
}


## cacheSolve uses the arguments provided above to calculate the inverse or recover it from cache if it is possible. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invM <-x$getInv()
	if(!is.null(InvM)){
		message("getting data from cache")
		return (InvM)
	}
	mat <- x$get()
	InvM <- solve(mat, ...)
	x$setInv(invM)
	invM	
}
