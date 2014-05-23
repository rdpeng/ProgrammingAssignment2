## These functions create a matrix using the data you supply, cache the inversed matrix
## and print the inversed matrix. The cached matrix is returned for repeated "gets".


## This function creates the matrix object and can cache the inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverseMatrix <- function(matrix) m <<- matrix
    getInverseMatrix <- function() m
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}

## This function computes the inverse of the matrix returned by makeCacheMatrix. 
## The function wil return the matrix from the cache if it has already been calculated.

cacheSolve <- function(x, rows, cols) {
    m <- x$getInverseMatrix()
    if(!is.null(m)) {
        message("Cached inversed matrix.")
        return(m)
    }
    data <- x$get()
    m <- matrix(data, rows, cols)
    inverse <- solve(m)
    x$setInverseMatrix(inverse)
    message("Inversed matrix.")
    inverse
}

