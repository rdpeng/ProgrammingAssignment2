## Two functions for matrix inversion computation.
## The result is stored in the cash.

## First function creates a special "matrix" object,
## that can cash its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL # s will be the inverse of a matrix, 
                  # reset every time makeCache matrix is called 
        
        get <- function() {x} # returns the original matrix
        
        setinverse <- function(solve) { s <<- solve} #computes the inverse 
                                                     #and stores it cashed
        
        getsolve <- function() { s } # return the cashed value
        
        list (get = get,
              setinverse = setinverse,
              getsolve = getsolve)
        #list indicates how to access the internal functions
        
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been calculated,
## the cacheSolve returns the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        # check if the s (matrix inverse) has already been calculated
        # if so, return it from the cash
        
        s <- x$getsolve()
        
        if(!is.null(s)) { 
                
                message("getting cashed data")
                
                return(s)
        }
        
        # if not, compute it.
        
        data <- x$get()
        
        s <- solve(data, ...)
        
        x$setinverse(s)
        
        s
        
        # Return a matrix that is the inverse of 'x'
}
