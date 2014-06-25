## These methods allow for the caching of the inverse of a matrix.
## This allows for a great efficiency increase on a stable matrix.

## Set up the methods and variables enabling the cached inverse
makeCacheMatrix <- function(x = matrix()) {
    ## The inverse is null until otherwise set
    i <- NULL
    
    ## The setter for the stored matrix
    set <- function(y){
        ## Store the new value
        x <<- y
        ## Reset the inverse to null
        i <<- NULL
    }
    
    ## Return the matrix
    get <- function() x
    
    ## Set the inverse
    setinv <- function(inv) i <<- inv
    
    ## Get the inverse
    getinv <- function() i
    
    ## Create the list of functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Solves the inverse of the matrix and caches the result
cacheSolve <- function(x, ...) {
    ## Get the current value of the inverse
    i <- x$getinv()
    
    ## If it is not null, it has already been cached, return that value.
    if(!is.null(i)){
        message("getting chached inverse")
        return(i)
    }
    
    ## No else since there is a return in the if statement
    
    ## Get the matrix
    data <- x$get()
    
    ## Solve the inverse of the matrix
    i <- solve(data, ...)
    
    ## Cache the value of the inverse
    x$setinv(i)
    
    ## Return the inverse
    i
}