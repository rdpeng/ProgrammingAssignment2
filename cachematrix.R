## Functions to create and compute the inverse of a matrix in a way it can be 
## cached to potentially avoid time-consuming computations

## A function that creates a special matrix object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## A function that returns the inverse of the special matrix object created 
## with the funcion makeCacheMatrix. It first checks to see the the inverse has
## already been calculated

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    inv
}
