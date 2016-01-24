## As we all known, calculating matrix inversion is often a time-consuming job.
## In order to save time,  the following functions are designed to create a special object that 
## stores a matrix and caches its inverse.
 
## Write a short comment describing this function
## This function can create a special "matrix" which can cache its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y) {
                x <<- y
                invm <<- NULL     
        }
        get <- function() x
        setInverseMatrix <- function(inverse) invm <<- inverse
        getInverseMatrix <- function() invm
        list(set = set, get = get,
                setInverseMatrix = setInverseMatrix,
              getInverseMatrix = getInverseMatrix)

}



## Following function is to get the inverse of the special "matrix" created BY
## makeCacheMatrix. If the inverse has already been here and the special matrix 
## not changed, then it gets the inverse matirx from the cache and skip the 
## computation. Otherwise, it computes the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invm <- x$getInverseMatrix()
        if(!is.null(invm)) {
                message("getting the cached Inverse Matrix")
                return(invm)
        }
        
        mat <- x$get()
        invm <- solve(mat, ...)
        x$setInverseMatrix(invm)
        invm
}
