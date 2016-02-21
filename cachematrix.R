## Put comments here that give an overall description of what your
## functions do

## This function create a special numeric matrix

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inversematrix) x_inv <<- inversematrix
    getinverse <- function() x_inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function return the Cached inverse of the matrix. If there is no cached inverse, it will 
## create an inverse and cache it

cacheSolve <- function(x, ...) {
    x_inv <- x$getinverse()
    if(!is.null(x_inv)) {
        message("getting cached inverse matrix")
        return(x_inv)
    }
    data <- x$get()
    x_inv <- solve(data, ...)
    x$setinverse(x_inv)
    x_inv
}
