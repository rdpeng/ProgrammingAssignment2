## The functions calculates the inverse of an matrix, and caches it for later use.

## This function creates a special "matrix" object that can cache its inverse. It will return a list of functions: getMatrix(), setMatrix(), getInverse(), setInverse()

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL;
	getMatrix <- function(){
		x;
	}
	setMatrix <- function(y = matrix()){
		x <<- y;
	}
	getInverse <- function(){
		inverse;
	}
	setInverse <- function(y = matrix()){
		inverse <<- y;
	}
	list(getMatrix = getMatrix, setMatrix = setMatrix, getInverse = getInverse, setInverse = setInverse);
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix() above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve() should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	if(!is.null(x$getInverse())){
		message("getting cached data.")
		return(x$getInverse());
	}else{
		x$setInverse(solve(x$getMatrix(),diag(nrow = dim(x$getMatrix())[1]),...));
	}
	x$getInverse();
}
