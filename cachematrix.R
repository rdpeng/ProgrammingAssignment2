## The functions below optimize the Solve (inverting a matrix) operation to cache the result so when the solve function is 
## called multiple times for the same matrix, the solve operation is done once.

## This function takes a matrix and gives back a set of funtions on this matrix. The output funcions allows us to get or set the matrix
## and get and set the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	inversedMatrixCache <- NULL
        set <- function(Y) {
                x <<- Y
                inversedMatrixCache <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) inversedMatrixCache <<- Inverse
        getInverse <- function() inversedMatrixCache
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function takes in a matrix, gets the inverse of it if it is cached. 
## If the inverse is not cached, the invesrse is calculated and cached.

cacheSolve <- function(x, ...) {
        inversedMatrixCache <- x$getInverse()
        if(!is.null(inversedMatrixCache)) {
                message("getting cached data")
                return(inversedMatrixCache)
        }
        data <- x$get()
        inversedMatrixCache <- solve(data)
        x$setInverse(inversedMatrixCache)
        inversedMatrixCache
}
