## Purpose: To return the inverse of a matrix (cached or not). In case the matrix is already cached, it is just retrieved, otherwise it is calculated. 
## The script consists of two functions:
##      1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##      2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##         If the inverse has already been calculated (and the matrix has not changed), 
##         then cacheSolve should retrieve the inverse from the cache.


## MakeCacheMatrix
## Purpose: Provides numerous functions for the matrix. 
## Input: a matrix (invertible)
## Output: a list of functions:
##     1. get: For getting the value of the matrix.
##     2. setInv: For inversing the matrix.
##     3. getInv: For getting the value of the inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                       
        get <- function() x
        setInv <- function(inv) m <<- inv     
        getInv <- function() m
        list(get=get, setInv=setInv, getInv=getInv) 
}

## cacheSolve
## Purpose: Check if the input matrix is already inverted. If so, the matrix is just cached, 
##          otherwise the inverted matrix is calculated.
## Input: a matrix (invertible)
## Output: the cached matrix + a message OR the inverted matrix +a msg


cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)) {
                message("Getting cached data...")
                return(m)
                
        }
        else{
                message("No cached data...Calculating...")
                data <- x$get()
                m <- solve(data)
                x$setInv(m)
                return(m)
                
               
        } 
        
}


## SAMPLE RUN
## - First run to get the calculated inverted matrix, e.g.
##      1. x <- matrix(sample(1:400), nrow=20, ncol=20)
##      2. y <-  makeCacheMatrix(x)
##      3. cacheSolve(y)
## Expected results: - The message: No cache data...Calculating...
##                    - The inverted matrix        
## - Then run again cacheSolve(y) in order to get the cached results.
## Expected results: - The message: Getting cached data...
##                   - The cached inverted matrix (same as before but you will get it much faster)

## ATTENTION: "Error in solve.default(data) :Lapack routine dgesv: system is exactly singular:..." means that the matrix is not invertible.You should provide another one.