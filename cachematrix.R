## Put comments here that give an overall description of what your
## functions do

# Write a short comment describing this function

# This function takes in a matrix e.g.
# x <- matrix(c(4,3,2,1,0,0,0,4,3), nrow = 3, ncol = 3)

makeCacheMatrix <- function(x = matrix()) {
    # takes in Matrix and assign to 'set'
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    # inverse Matrix and cache it
    get <- function() x #why x outside of bracket?
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

# why need a second function?

cacheSolve <- function(x, ...) {
    #getInverseMatrix
    #if InverseMatrix calculated, retrieve the inverse, else inverse it
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}