makeCacheMatrix <- function(x = numeric()) {
	#initializing inv as empty
        inv <- NULL
	#sets x equal to y in the parent environment
	#clears inv from any previous calculations
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
	#grabs a created x from the parent environment (m1 and n2 in the examples)
	#uses lexical scoping
        get <- function() x
	#defines inv variable as the solve function which takes the inverse of a matrix
	#uses the get function like above to grab inv from the parent environment
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
	#returns these functions as a list to the parent environment
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
#makeCacheMatrix MUST be called before cacheSolve, otherwise cacheSolve doesn't know
#inv and getinverse

cacheSolve <- function(x, ...) {
	#functions calls getinverse from parent environment
        inv <- x$getinverse()
	#should be null if ran makeCacheMatrix but if previous run is still valid
	#this returns the inverse stored in the parent environment
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
	#if inv is not null then calls the functions in makeCacheMatrix to return
	#inverse.  Inverse is calculated through makeCacheMatrix, NOT here
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
