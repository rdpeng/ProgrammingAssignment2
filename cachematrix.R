## Matrix inversion is usually a costly computation
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   inverseMatrix <- NULL
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        get <- function()
			x
        setInverse <- function(inverse)
			inverseMatrix <<- inverse
        getInverse <- function()
			inverseMatrix
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## This function computes the inverse of the special "matrix" created by makeCacheMatrix 
## If the inverse has already been calculated then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		      inverseMatrix <- x$getInverse()
        if (!is.null(inverseMatrix)) {
		message("Inverse Matrix Already Constructed...")
                return(inverseMatrix)
        }
        matrixValue <- x$get()
        inverseMatrix <- solve(matrixValue, ...)
        x$setInverse(inverseMatrix)
        inverseMatrix
}
