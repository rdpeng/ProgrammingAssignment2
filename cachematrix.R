
##Catching the inverse of a Matrix.
## This function creates a speacil "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

}


## The following function calculates the mean of the special "vector" created with the above function. 

cacheSolve <- function(x, ...){
        ## Return a matrix that is the inverse of 'x'
        inv <-x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
