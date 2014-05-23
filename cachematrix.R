## The following functions take a matrix (assumed to be invertible), 
## compute its inverse, and save both to memory.
## This is done to minimize the return time of a matrix inverse,
## as repetition of the 'solve' function can be quite costly.
## makeCacheMatrix & cacheSolve "communicate" through a list of functions 
## created by makeCacheMatrix.

## makeCacheMatrix creates a special "matrix" that is an interface
## (whose elements are functions: set, get, setInverse, getInverse)
## to a cached matrix.
## 
## internal functions:
##      set -- replaces previous matrix 'x' with new matrix 'y', 
##             and resets the 'inverse' value to NULL
##      get -- returns the matrix saved in memory
##      setInverse -- sets 'inverse' to be the value received
##      getInverse -- returns the inverse of the matrix saved in memory

makeCacheMatrix <- function(x = matrix()) {
        
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(solvedInv) inverse <<- solvedInv
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## cacheSolve takes the function interface created by makeCacheMatrix,
## and -- (for the original matrix received by makeCacheMatrix) -- either
##      (1) accesses the cache and returns the previously computed inverse; or 
##      (2) computes and returns the inverse of the original matrix.

cacheSolve <- function(interface, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inverse <- interface$getInverse()
        
        ## Check if inverse has already been computed...
        if(!is.null(inverse)) {
                
                ## ...if so, return inverse from cache.
                message("getting cached data")
                return(inverse)
        }
        ## ...if not, calculate matrix inverse, then return it.
        message("calculating matrix inverse")
        data <- interface$get()
        inverse <- solve(data, ...)
        interface$setInverse(inverse)
        inverse
}