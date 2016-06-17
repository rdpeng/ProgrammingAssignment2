## Below are two functions that are used to create a special object
## that stores a numeric matrix and cache's its invers.

# makeCacheMatrix creates a special "matrix",
# which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse matrix
# 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse.matrix <- NULL
        set <- function(y) {
                x <<- y
                inverse.matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(solve.matrix) inverse.matrix <<- solve.matrix
        getinverse <- function() inverse.matrix
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}

# The following function calculates the inverse matrix of the special "matrix" 
# created with the function makeCacheMatrix. However, it first checks to see 
# if the inverse matrix has already been calculated. If so, it gets the 
# inverse matrix from the cache and skips the computation. 
# Otherwise, it calculates the inverse matrix of the matrix and sets 
# the value of the inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse.matrix <- x$getinverse()
        if(!is.null(inverse.matrix)) {
                message("getting cached data")
                return(inverse.matrix)
        }
        inverse.matrix <- solve(x$get(), ...)
        x$setinverse(inverse.matrix)
        inverse.matrix
}