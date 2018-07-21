## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## This function is used to generate a matrix that can has its inverse
              MatrixInverse <- NULL. ##initialise MatrixInverse as NULL
              MatrixSetting <- function(y) {  ## define the MatrixSetting function to assign the new values of the matirx
                x <<- y
                MatrixInverse <<- NULL    ## if there is a new matrix, reset MatrixInverse to NULL
        }
        MatrixGetting <- function(x) 
        InverseSetting <- function(inverse) ## assgin value of MatrixInverse in parent environemnt
        MatrixInverse <<- inverse
        InverseGetting <- function(MatrixInverse)
        list(MatrixSetting = MatrixSetting, MatrixGetting = MatrixGetting, InverseSetting = InverseSetting,
             InverseGetting = InverseGetting)

}


## Write a short comment describing this function
## This function cumputes the inverse of the matrix returned by makeCacheMatrix above.
## If the inverse has alreadt been calculated or the matrix has not changed, then cacheSolve will retrieve the   
## inverse from the cache
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
