## These functions cache and retrieve the inverse of a matrix

## makeCacheMatrix sets/gets the value of the matrix,
## and sets/gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve checks to see whether the inverse of the matrix has
## already been calculated. If it has, it retrieves the cached
## inverse calculation. If it has not, it solves the inverse of the 
## matrix.

cacheSolve <- function(x, ...) {
        ## First, checks for cached inverse and returns if exists
    inv <- x$getinv()
    if(!is.null(inv)) { 
        message("getting cached data")
        return(inv)
    }
        ## If no cached inverse, solves inverse
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
