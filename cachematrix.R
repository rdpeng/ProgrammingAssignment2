## A pair of functions that cache the inverse of a matrix.

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        invMatrix <- NULL
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        get <- function() x
        setInverse <- function(solveMatrix) invMatrix <<- solveMatrix
        getInverse <- function() invMatrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        invMatrix <- x$getInverse()
        if(!is.null(invMatrix)) {
                message("getting cached data")
                return(invMatrix)
        }
        data <- x$get()
        invMatrix <- solve(data)
        x$setInverse(invMatrix)
        invMatrix

        
}
