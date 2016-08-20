## Takes & Caches a matrix and prepares for the inversing with a list of functions
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() inverseMatrix
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Inverses the matrix previously cached and returns the inverse of that cached matrix
cacheSolve <- function(x, ...) {
    inverseMatrix <- x$getInverse()
    
    if(!is.null(inverseMatrix)) {
        message("getting cached data.")
        return(inverseMatrix)
    }
    
    matrixValues <- x$get()
    inverseMatrix <- solve(matrixValues)
    x$setInverse(inverseMatrix)
    
    return(inverseMatrix)
}
