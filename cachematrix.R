# The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function (x = matrix()) {
inv <- NULL
set <- function (y) {
   x <<- y
   inv <<- NULL
}
get <- function()x
setinverse <- function(inverse) inv <<- inverse
getinverse <- function() inv
list (set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse )
}




# The following function returns the inverse of the matrix. It first checks if
# the inverse has beed computed and cached. If yes, the it gets the result and skips the
# computation. If not, then it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
 	if (!is.null(m)) {
                message ("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data,..) 
	x$setmean(m)
	m
}
