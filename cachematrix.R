## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly.
## Here is a pair of functions that cache the inverse of a matrix:
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix function.
##                If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.


## This function creates a special "matriz", which is really a list containing a function to
## 1 .set the value of the matrix
## 2. get the value of the matix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    setMatrix <- function(pMatrix) {
        x <<- pMatrix
        inverseMatrix <- NULL
    }
    getMatrix <- function() {
        x
    }
    setInverseMatrix <- function(pInverseMatrix) {
        inverseMatrix <<- pInverseMatrix
    }
    getInverseMatrix <- function() {
        inverseMatrix
    }
    list(setMatrix=setMatrix, getMatrix=getMatrix, setInverseMatrix=setInverseMatrix, getInverseMatrix=getInverseMatrix)
}


## 
## The following function computes the inverse of the special "matrix" created with the cacheMatrix function. 
## However, it first checks to see if the inverse matrix has already been calculated (and the matrix has not changed). 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix of the data and sets the value of the inverse matrix in the cache via the setinverse function.
## 
cacheSolve <- function(x, ...) {
    inverse <- x$getInverseMatrix()
    if (!is.null(inverse)) {
        message("get inverse matrix from cache data")
        return (inverse)
    }
    matrix <- x$getMatrix()
    inverse <- solve(matrix)
    x$setInverseMatrix(inverse)
    
    ## Return a matrix that is the inverse of 'x'
    inverse
}
