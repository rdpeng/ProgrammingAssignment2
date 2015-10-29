## Caching the Inverse of a Matrix with two functions 
## makeCacheMatrix and cacheSolve

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        setvalue <- function(y){
                x<<- y
                s <<- NULL
        }
        getvalue <- function() x
        setsolve <- function(solve) s<<- solve
        getsolve <- function(solve) s
        list(setvalue = setvalue, getvalue = getvalue, 
             setsolve = setsolve, getsolve = getsolve)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$getvalue()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
