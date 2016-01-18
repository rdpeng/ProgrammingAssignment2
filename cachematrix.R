## Create a list of matrix functions for the input matrix
## Cache values as a new matrix is given

## Create a matrix (assign to a variable - Ex: "v")
## Pass that variable to this function, assigning the result to a new variable - Ex: "w"

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Solve the input matrix using the functions from makeCacheMatrix
## Use cached values when possible

# Use the variable created when calling "makeCacheMatrix" - Ex: "w"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
