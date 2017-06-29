## Functions that cache the inverse of the matrix

## This function creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
        
    }
    get <- function() x 
    setInverse <- function(solveMatrix) inv <<- solveMatrix
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the "matrix" returned by the makeCacheMatrix above

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
