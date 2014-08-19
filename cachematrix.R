## Put comments here that give an overall description of what your
## functions do

## This function includes the 4 functions used in this assignment.
## It allows us to set and retrieve(get) the matrix itself 
## and to set and retrieve (get) it's inverse).
## By resetting the 'i' variable within the set functon 
## (using the superassignment operator) we can prevent 
## previous solutions from showing up incorrectly. 

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y){
		x <<- y
		i  <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) {
		i <<- inv
		return(i)
	}
	getinverse <- function() i
	list (set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
	
}


## This function checks for a previous sollution using the 
## getinverse function and if none exists, solves the inverse 
## and stores it using the setinverse function.
## In either case it returns the inversed matrix 
## (though through diffrent means).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)) {
		    message("getting cached data")
                return(i)
	}
	data <- x$get()
	i <- solve(data)
	x$setinverse(i)
	i
}
