## Two functions in this file; makeCacheMatrix and cacheSolve.
##
## makeCacheMatrix will accept a matrix as an argument and load cache with a object "matrix"
## with the inverse of the matrix.  It will set this every time.
##
## cadheSolve will check to see if the inverse for the input matrix has been calculated and 
## planced in cache.  If so it will output that cache, if not it will calcuate the inverse.

## Sets the inverse of the matrix into "matrix" every time.

makeCacheMatrix <- function(x = matrix()) {
        
        matrix <<- NULL
        matrix <<- solve(x)
        
}



## Checks the cache "matrix" to the input.  If it matches, then it will take form cache.  If not
## it creates the inverse.  Print messages added for demonstration purposes.  A possible enhancement
## could be to load the cache from this function, but that was not in the requirements.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if (identical(x, matrix) == TRUE) {
                print("retreiving content from cache")
                matrix
        }
        else {
                print("calculating inverse")
                solve(x)
                ##matrix <<- solve(x)
                ##matrix
        }
}
