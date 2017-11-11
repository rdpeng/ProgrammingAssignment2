##  Caching Inverse of the matrix

## makeCacheMatrix function caches the inverse matrix. This method
## also creates a series of get and set method.

makeCacheMatrix <- function(x = matrix()) {
    inverseMat <- NULL
    setMat <- function(y) {
        x <<- y
        inverseMat <<- NULL
    }
    getMat <- function() x
    setInverseMat <- function(invMat) inverseMat <<- invMat
    getInverseMat <- function() inverseMat
    list(setMat = setMat, getMat = getMat,
         setInverseMat = setInverseMat,
         getInverseMat = getInverseMat)
}


## cacheSolve function returns the inverse of the input matrix. 
## It checks if the inverse has already been computed. If it is 
## computed, it returns result stored in cache. Else computes inverse
## stores it in cache and returns result.

cacheSolve <- function(x, ...) {
    invMatrix <- x$getInverseMat()
    if(!is.null(invMatrix)) {
            message("Getting inverse Matrix from cache")
            return(invMatrix)            
    }
    invMatrix <- x$getMat()
    invMatrix <- solve(invMatrix)
    x$setInverseMat(invMatrix)
    
        ## Return a matrix that is the inverse of 'x'
    invMatrix
}
