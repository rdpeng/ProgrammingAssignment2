## Since matrix inversion is computationally expensive, the functions below
## address this by caching the inverse of a given matrix and retrieving it
## as necessary to avoid calculating the inverse multiple times.

## Constructs a "cache matrix" consisting of a list of set and get functions
## along with functions to setinverse and getinverse to set and retrieve the
## inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}


## If the inverse of the cached matrix has been stored, this function retrieves
## it. If not, it gets the matrix andsets the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
