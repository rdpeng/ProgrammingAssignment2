## Put comments here that give an overall description of what your
## functions do

## Function to set up a matrix and cache it

makeCacheMatrix <- function(x = matrix
                            ()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Function to inverse the given matrix

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if (!is.null(m)) {
                return (m)
        }
        data <- x$get()
        m <- solve(data)
        x$setmatrix(m)
        m
}
