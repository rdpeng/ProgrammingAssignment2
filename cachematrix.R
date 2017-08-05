## The following two functions allow the user to store the inverse of a matrix
## in the cache so that R does not have to compute this solution every time

## makeCacheMatrix initializes the data in matrix form. This function provides
## two help functions called set and solve, which allow the user to specify the 
## inverse of a matrix or retrieve it, respectively. 
## Example:
## dat <- matrix(rnorm(16) ncol = 4)
## makeCacheMatrix(dat)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve computes the inverse of a matrix from a makeCacheMatrix() object.
## Example:
## cacheSolve(dat)

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
