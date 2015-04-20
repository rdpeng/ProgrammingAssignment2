# Below are two functions that are used to create a special object that stores a matrix and caches its inverse.

# The makeCacheMatrix function, creates a special "matrix", which is really a list containing functions to:
# (1) Set the values of the matrix
# (2) Get the values of the matrix
# (3) set the inverse of the matrix
# (4) Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
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

# The cacheSolve function calculates the inverse of the special "matrix" created with the above function.
# However, it first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache, via 
# the setinverse function.

cacheSolve <- function(x, ...) {
        # Returns a matrix that is the inverse of 'x'
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
