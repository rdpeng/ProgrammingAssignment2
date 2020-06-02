## A few funtions that cache the inverse if a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set<-function(y){
		x<<-y
		inv<<-NULL
	}
	get<-function() x
	setinverse<-function(solveMatrix) inv<<-solveMatrix
	getinverse<-function() inv
	list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
        	message("getting cached data")
        	return(inv)
        }
        data <- x$get
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
