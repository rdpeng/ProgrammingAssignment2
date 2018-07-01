## Put comments here that give an overall description of what your
## functions do

## create a special object that stores a matrix

makeCacheMatrix <- function(x = matrix()) {
 invers <- NULL
        set <- function(y) {
                x <<- y
                invers <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invers <<- inverse
        getinverse <- function() invers
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## calculates the inverse of the special "matrix" created with the above function

cacheSolve <- function(x, ...) {
        invers <- x$getinverse()
        if(!is.null(invers)) {
                message("getting cached data")
                return(invers)
        }
        data <- x$get()
        invers <- solve(data, ...)
        x$setinverse(invers)
        invers
}
