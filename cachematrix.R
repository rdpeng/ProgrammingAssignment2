# Matrix inversion is often a costly computation. 
# This is a pair of functions that cache the inverse of a matrix

## The first function, makeCacheMatrix, is function to
1. set the value of the matrix
2. get the value of the matrix
3. set the value of the inverse of the matrix
4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y) {
			x <<- y
			inv <<- NULL
		}
		get <- function() x
		setinv <- function(inverse) inv <<- inverse
		getinv <- function() inv
		list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This second function, cacheSolve, computes the inverse of the matrix
# returned by makeCacheMatrix from above. If the inverse has already been
# calculated and the matric has not changed, then the cachesolve will 
# retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
        	message("getting cached data")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}

