## The following pair of functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	## Create a special matrix object and store its inverse in cache
	 m <- NULL
        ob1 <- function(y) {
                y <- solve(x)
                x <<- y
                m <<- NULL
        }
        ob2 <- function() x
        list(ob1 = ob1, ob2 = ob2)
}


## This function checks to see whether an invberse of the matrix already exists,
## .calls it if it does, and calculates it if it does not already exist

cacheSolve <- function(x, ...) {
 	m <- NULL
        ob1 <- function(y) {
                y <- solve(x)
                x <<- y
                m <<- NULL
        }
        ob2 <- function() x
        list(ob1 = ob1, ob2 = ob2)
}
