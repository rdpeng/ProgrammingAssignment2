## Code developed for the 'Programming Assignment 2' of the 'R Programming' course.
## The following functions should do the caching of the inverse of a matrix,
## in order to reduce the computational complexity of performing unecessary
## calculations of already defined data.

## Function name: makeCacheMatrix 
## Description: The function 'makeCacheMatrix' creates a special type of vector, composed by 
##				functions to:
##					-> set the value of the matrix;
##					-> get the value of the matrix;
##					-> set the value of the inverse matrix (cache);
##					-> get the value of the inverse matrix.
##Parameters:
##	-> x: a matrix to store/calculate its inverse.

makeCacheMatrix <- function(x = matrix()) {
	invmat <- NULL
	
	set <- function(y){
		x <<- y
		invmat <<- NULL
	}
	
	get <- function(){
		x
	}
	
	setinv <- function(inv){
		invmat <<- inv
	}
	
	getinv <- function(){
		invmat
	}
	
	list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function name: cacheSolve 
## Description: The function 'cacheSolve' returns a matrix that is the inverse of 'x'
##Parameters:
##	-> x: a special vector composed by functions to get/set a matrix
##		  and to get/set its inverse.

cacheSolve <- function(x, ...) {
		invmat <- x$getinv()
        if(!is.null(invmat)) {
                message("getting cached inverse matrix")
                return(invmat)
        }
        origmat <- x$get()
        invmat <- solve(origmat, ...)
        x$setinv(invmat)
        invmat
}
