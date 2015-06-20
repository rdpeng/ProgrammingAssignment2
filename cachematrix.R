## The 'makeCacheMatrix' function creates a special "matrix" object that can cache its inverse.
## The first two 'if' statements specify the input should be a squared matrix

makeCacheMatrix <- function(x = matrix()) {
        if(!is.matrix(x)) stop("input must be a matrix")
	if(nrow(x)!= ncol(x)) stop("input must be a squared matrix")
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function(){
		x
	}
	setinverse <- function(inverse){
		inv <<- inverse
	}
	getinverse <- function(){
		inv
	}
	
	list(set = set, get = get, 
		setinverse = setinverse,
		getinverse = getinverse)

}


## The 'cacheSolve' function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
