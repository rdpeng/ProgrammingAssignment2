# Compute the inverse of a matrix is cost. 
# This two functions are used to cache this calculation.

# This function creates a special "matrix" object that can cache its inverse.
# First, the value of the matrix is set
# Then, get the value of the matrix
# After this, the value of inverse of the matrix is set
# Last, get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# This function returns the inverse of the matrix. 
# It checks if the calculation was done before.
# To calculate the matrix the setinverse function is used. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinverse()
		if(!is.null(inv)) {
			message("getting cached data.")
			return(inv)
		}
		data <- x$get()
		inv <- solve(data)
		x$setinverse(inv)
		inv
}
