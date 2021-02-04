## The functions in this file cache the inverse of a matrix

## makeCacheMatrix(): Creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    
    # Stores the matrix in argument y to an object, x, that is in an environment
    # different from the current environment. Creates an object, inverseMatrix,
    # that will be used to store the inverse of the matrix in an environment
    # outside the current environment
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    
    # Gets the stored matrix
    get <- function() x
    
    # Stores the inverse of the matrix in an object, inverseMatrix, outside the
    # current environment
    setInverse <- function(solve) inverseMatrix <<- solve
    
    # Gets the stored inverse matrix
    getInverse <- function() inverseMatrix
    
    # Create and return the list of functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  
}


## cacheSolve(): As appropriate, computes the inverse of the matrix returned by
## makeCacheMatrix(). If the inverse has already been calculated and the matrix
## has not changed, the function retrieves the inverse from the cache
cacheSolve <- function(x, ...) {

    # If the inverse of the supplied matrix (x) is not empty and if the supplied
    # matrix is identical to the cached matrix (i.e.., the matrix hasn't
    # changed), display a message and return the cached inverse
    inverseMatrix <- x$getInverse()
    if((!is.null(inverseMatrix)) && (identical(x, x$get()))) {
        message("Returning cached matrix")
        return(inverseMatrix)
    }
    
    # Otherwise, cache the matrix and calculate, set, and return the inverse
    x$set(x)
    inverseMatrix <- solve(x, ...)
    x$setInverse(inverseMatrix)
    return(inverseMatrix)
}
