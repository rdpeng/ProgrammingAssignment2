## Below are 2 functions, makeCacheMatrix and cacheSolve, which cache the
## inverse of a matrix.

## The below function, makeCacheMatrix, creates a special matrix object that
## can cache its inverse.

makeCacheMatrix <- function(mx = matrix()) {
    mat <- NULL

    # Sets the matrix
    setMatrix <- function(matrix){
        mx <<- matrix
        mat <<- NULL
    }
    
    # Returns the matrix
    getMatrix <- function(){
        mx
    }
    
    # Inverts the matrix
    setInverse <- function(inverse){
        mat <<- inverse
    }
    
    # Returns the inverse of the matrix
    getInverse <- function(){
        mat
    }
    
    # Creates a list of the methods and returns it
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## The below function takes in a special matrix 'x' returned by the makeCacheMatrix function
## above, and computes its inverse. If the inverse of the matrix is already calculated and
## the matrix is unchanged, the below function can retrieve this inverse from the cache.

cacheSolve <- function(x, ...) {
    
    # Returns a matrix that is the inverse of 'x'
    mx <- x$getInverse()
    
    # Returns the inverse if already set
    if(!is.null(mx)){
        return(mx)
    }
    
    # Retrieves the matrix from 'x' and calculates the inverse
    matData <- x$getMatrix()
    mx <- solve(matData) %*% matData
    
    # Sets the inverse of 'x' and returns it
    x$setInverse(mx)
    mx
}
