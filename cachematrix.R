## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
              MatrixInverse <- NULL
              MatrixSetting <- function(y) {
                x <<- y
                MatrixInverse <<- NULL
        }
        MatrixGetting <- function(x) 
        InverseSetting <- function(inverse) 
        MatrixInverse <<- inverse
        InverseGetting <- function(MatrixInverse)
        list(MatrixSetting = MatrixSetting, MatrixGetting = MatrixGetting, InverseSetting = InverseSetting,
             InverseGetting = InverseGetting)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        MatrixInverse <- x$InverseGetting()
        if(!is.null(MatrixInverse)) {
                Message("Do you wang to get an invertible matrix?")
                return(MatrixInverse)
        }
        
        MatrixResult <- x$MatrixGetting()
        MatrixInverse <- solve(MatrixResult, ...)
        x$InverseSetting(MatrixInverse)
        return(MatrixInverse)
        ## Return a matrix that is the inverse of 'x'      
}
