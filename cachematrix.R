## The two functions below are designed to retrun the inverse of a matrix
## without the use of a loop. Using a loop can make the operation 
## very slow for big amounts of data. Caching instead can make this
## process more efficient by creating objects that are stored and retrieved
## easily when they are needed.

## This function take a matrix object (note that it has to be an invertible one)
## and creates objects where parts of it are stored. The function retruns a
## list of this objects.

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


## This second function is designed to be applied to an object obtained through
## the application of the first function. This second function first controlls
##if among the object in makeCacheMatrix is located an inverted matrix and
## it is not takes the get object from the previous function and gives its inverse

cacheSolve <- function(x, ...) {
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