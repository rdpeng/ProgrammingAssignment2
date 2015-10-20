makeCacheMatrix <- function(mat = matrix()) {

## Two Functions makeCacheMatrix and cacheSolve
## Written by Mathan Anto Marshine(pm13pmathan@iimidr.ac.in)

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## Cache matrix mat and its inverse invmat

invmat=NULL

setmat=function(y){
mat<<- y
invmat<<- NULL
}

getmat = function () mat

setinv = function(inv) invmat <<- inv 
getinv = function() invmat

list(setmat=setmat, getmat=getmat, 
setinv=setinv, getinv=getinv)

}


cacheSolve <- function(mat) {
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## Written by Mathan Anto Marshine(pm13pmathan@iimidr.ac.in)
## Return a matrix that is the inverse of 'x'

invmat = mat$getinv()
        

if (!is.null(invmat)){
    message("Inverse Matrix is available in Cache")
    return(invmat)
}

## Faced error Lapack routine dgesv: system is exactly singular:  for larger matrixes
## Error is due to sensitivity of the error. Hence used ginv
## used library MASS and Function ginv

library(MASS)
        
datamatrix = mat$getmat()
invmat = ginv(datamatrix)
##print(invmat)        
mat$setinv(invmat)
return(invmat)

}
