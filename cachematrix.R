
## Matrix inversion is usually a costly computation and their may be some benefit to
## of a matrix rather than compute it repeatedly (there are also alternatives to matrix
## inversion that we will not discuss here). Your assignment is to write a pair of 
## functions that cache the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	myInverse <- NULL
	
	set <- function (z) {
		x <<- z
		myInverse <<- NULL
	}
	
	get <- function() x
		
	setinverse <- function(inverse) myInverse <- inverse
	getinverse <- function() myInverse
	list(set=set,get=get,setinverse=setinverse, getinverse=getinverse)

}


## cacheSolve function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        myInverse <- x$getinverse()
        if (!is.null(myInverse)) {
        	message("Getting inverse data")
        	return(myInverse)
        }
        
        data <- x$get()
        
        myInverse <- solve(data)
        x$setinverse(myInverse)
        myInverse
}
