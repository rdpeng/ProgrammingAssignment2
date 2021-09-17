## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## A function makecachematirx has been created .

makeCacheMatrix <- function(x = matrix()) {
         t <- NULL
        set <- function(y) {
                t <<- y
                t<<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) t <<- Inverse
        getInverse <- function() t
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
##Cachesolve function has been created.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        t <- x$getInverse()
        if(!is.null(t)) {
                message("getting cached data")
                return(t)
        }
        data <- x$get()
        t <- Inverse(data, ...)
        x$setInverse(t)
        t
}
