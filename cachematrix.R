## The function makeCacheMatrix takes matrix as input parameter, and does the following :
## a. set the value of the matrix
## b. get the value of the matrix
## c. set the inverse of the matrix
## d. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    invMat <- NULL
    
    setMat <-function(y){
        x <<- y
        invMat <<- NULL
    }
    
    getMat <- function() x
    setInv <- function(inverse) invMat <<- inverse
    getInv <- function() invMat
    
    list(setMat = setMat, getMat=getMat,
    setInv=setInv, getInv=getInv)
    
}


## The function cacheSolve takes matrix as input parameters,
## Return the cached inverse matrix if exists, else get the matrix,
## calculate the inverse, and then return the matrix.

cacheSolve <- function(x, ...) {
    
    invMat<- x$getInv()
    
    ## Return the cached inverse matrix, if exists
    
    if(!is.null(invMat)){
        message("Invertible Matrix is Cached !!!")
        return(invMat)
    }
    
    ## Return the calculated inverse matrix if the inverse matrix is not cached.
    
    invMat <- solve(x$getMat(), ...)
    x$setInv(invMat)
    return(invMat)
    
}
