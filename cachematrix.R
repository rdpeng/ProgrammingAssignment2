## The functions in this script can help us looking for the inverse of a matrix from the cached data in order to save calculation time.
## If there is no cached data applied, the functions can also calculate for the inverse.


## The makeCacheMatrix function is a function which contains 4 functions to set and get a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setMatrix <- function(y){
                x <<- y
                inv <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(z) inv <<- z
        getInverse <- function() inv
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve function can get the inverse of a matrix from the cached data(if applied) or generate its inverse by solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        oriMatrix <- x$getMatrix()
        inv <- solve(oriMatrix, ...)
        x$setInverse(inv)
        inv
}
