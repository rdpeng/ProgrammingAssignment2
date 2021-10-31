## The function makeCacheMatrix creates a special matrix object that can cache its inverse
## and has four functions matrix operations: set, get, setInv, and get Inv.  The, set function 
## sets the matrix value, the get function obtains the matrix value, 
## the setInv function sets the matrix value, and getInv gets the matrix value.

makeCacheMatrix <- function(x = matrix()) {
    invResult <- NULL
    
    set <- function(y){
        x <<- y 
        invResult <<- NULL
    }
    get <- function(){
        x
    }
    setInv <- function(inverse) {inv <<- inverse}
    getInv <- function() {
        invResult
    }
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## The cacheSolve function calculates the inverse of the special matrix object
## return from makeCacheMatrix function.  First a check is done to see if
## the inverse operation has been already performed and matrix object not change.  
## If so it gets the inverse from cache and returns the inverse result.  Otherwise,
## the inverse operation is performed and sets the inverse value in 
## cache through the setInv function.  The solve function is use to compute inverse
## a square matrrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invResult <- x$getInv()
    if(!is.null(invResult))
    {
        message("getting cached data")
        return(invResult)
    }
    mat <-x$get()
    invResult <- solve(mat, ...)
    x$setInv(invResult)
    invResult
}