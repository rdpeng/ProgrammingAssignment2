# The following creates a temporary cache to get ready to find the inverse of a matrix 
# " <<- " is used to set the value in a different environment.
makeCacheMatrix <- function(x = matrix()){
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# The following function checks if the matrix is the inverse if not it sets "midstep"
# as the matrix and uses the solve function to find its inverse.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        midstep <- x$get()
        m <- solve(midstep, ...)
        x$setinverse(m)
        m
}