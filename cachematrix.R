## Two functions in this file; makeCacheMatrix and cacheSolve.
##
## makeCacheMatrix will accept a matrix as an argument and load cache with an object "matrix"
## that is an empty matrix.  It will then return the original matrix that was passed into the
## function.
##
## cadheSolve will check to see if the inverse has been calcualted by checking "matrix" for data.
## If it finds data it will return that value, if not it will calcuate the inverse of the input matrix.

## Creates the cache "matrix" and returns the input matrix data.

makeCacheMatrix <- function(x = matrix()) {
        
        matrix <<- matrix()
        return (x)
        
}



## Checks the cache for values.  If it exists, then it will take form cache.  If not
## it creates the inverse and sets the cache.  Print messages added for demonstration purposes. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if (is.na(matrix[1]) == TRUE) {
                print("calculating inverse")
                matrix <<- solve(x)
                matrix
        }
        else {
               
                print("retreiving content from cache")
                matrix
        }
}
