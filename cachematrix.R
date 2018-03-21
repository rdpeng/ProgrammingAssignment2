## The following pair of functions, makeCacheMatrix and cacheSolve, enables
## one to cache the values of a matrix and its inverse, updating either
## as needed

## makeCacheMatrix() houses the cahced values of the matrix and its inverse
## while providing 'get' and 'set' functions for both objects

## x is the input matrix that will be stored in the makeCacheMatrix environment

## Returns a list of get/set functions enabling one to retrieve or manipulate
## the values of the matrix and its inverse stored within the function
## environment

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    
    set <- function(y){
        x <<- y
        x_inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) x_inv <<- inverse
    getInv <- function() x_inv
    
    list(set = set, get = get,
         setInv = setInv, getInv = getInv)
}


## cacheSolve is the 'partner' function to makeCacheMatrix() that monitors
## and updates the value of the inverse cached in makeCacheMatrix

## x is a makeCacheMatrix() object

## Returns the inverse of the cached matrix and updates the cached value 
## of the inverse, if necessary

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)){
        print("retrieving cached value")
        return(inv)
    }
    
    print("calculating new value")
    inv <- solve(x$get(), ...)
    x$setInv(inv)
    inv
}
