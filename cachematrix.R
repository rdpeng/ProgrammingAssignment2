## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {
        x
    }
    setInv <- function(invMatrix){
        inv <<- invMatrix
    }
    getInv <- function() {
        inv
    }
    
    ##return
    list(set = set, get = get,
         setInverse = setInv,
         getInverse = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ##Get the inverse
    xInv = x$getInverse()
    ##If the inverse has already been calculated
    if(!is.null(xInv)){
        message("getting cached data")
        return(xInv)
    }
    
    ##else
    data <- x$get()
    ## calculate inverse
    xInv <- solve(data, ...)
    x$setInverse(xInv)
    xInv
}
