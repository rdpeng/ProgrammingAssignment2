## This program is made for getting the inverse of a matrix. It's going to be a
## "two steps" (functions) program.


# On this first function, we're going to create a list that contains the
# subfunctions we are going to use on the next function.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        # First function: set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # Second Function: get the value of the matrix
        get <- function() {
                x
        }
        # Third Function: set the valor of the inverse of the matrix
        setinverse <- function(solve) {
                inv <<- solve
        }
        # Fourth Function: get the valor of the inverse of the matrix
        getinverse <- function() {
                inv
        }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# On this function we're going to calculate the inverse of the matrix, assuming
# that the matrix supplied will always be invertible.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv # We finally return the inverse of the matrix
}