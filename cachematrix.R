## The functions in this file cache the inverse of a matrix

## makeCacheMatrix(): Creates a matrix object that can cache its inverse
makeCacheMatrix <- function(mat = matrix()) {
    inverseMat <- NULL
    
    # Stores the matrix in argument y to an object, mat, that is in an
    # environment different from the current environment. Creates an object,
    # inverseMatrix, that will store the inverse of the matrix, also in an
    # environment outside the current environment
    set <- function(y) {
        mat <<- y
        inverseMat <<- NULL
    }
    
    # Returns the cached matrix
    get <- function() {
        mat
    }
    
    # Caches solved, the inverse of the matrix
    setInverse <- function(solved) {
        inverseMat <<- solved
    }
    
    # Returns the cached inverse matrix
    getInverse <- function() {
        inverseMat
    }
    
    # Create and return the list of functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  
}


## cacheSolve(): As appropriate, computes the inverse of the matrix returned by
## makeCacheMatrix(). If the inverse has already been calculated and the matrix
## has not changed, the function retrieves the inverse from the cache
cacheSolve <- function(mat, ...) {

    # If the inverse of the supplied matrix (x) is not empty and if the supplied
    # matrix is identical to the cached matrix (i.e.., the matrix hasn't
    # changed), display a message and return the cached inverse
    inverseMatrix <- mat$getInverse()
    if((!is.null(inverseMatrix)) && (identical(mat, mat$get()))) {
        message("Returning cached matrix")
        return(inverseMatrix)
    }
    
    # Otherwise, cache the matrix and calculate, set, and return the inverse
    mat$set(mat)
    inverseMatrix <- solve(mat, ...)
    mat$setInverse(inverseMatrix)
    return(inverseMatrix)
}
