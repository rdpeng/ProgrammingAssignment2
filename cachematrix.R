## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    get <- function() x
    set <- function(mtx) {
        x <<- mtx
        inverse <<- NULL
    }
    getInverse <- function() inverse
    setInverse <- function(inv) inverse <<- inv
    list(get = get, set = set,
         getInverse = getInverse, setInverse = setInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse2 <- x$getInverse()
    
    if(!is.null(inverse2)) {
        message("Returning cached inverse.")
        return(inverse2)
    }
    
    data <- x$get()
    inverse2 <- solve(data, ...)
    x$setInverse(inverse2)
    inverse2
}


