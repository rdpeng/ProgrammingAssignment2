## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## create special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL     ## Initialize the matrix inverse to NULL
    SetData <- function(y) {
        x <<- y   ## x is assigned the value of y
        m <<- NULL ## matrix inverse is initialized to NULL
    }
    ## returns the original matrix x
    GetData <- function () x 
    ## matrix inverse is assigned the supplied value
    SetInverse <- function (z) m <<- z
    ## retrieve the matirx inverse m
    GetInverse <- function() m
    ## make this "matrix" into a list of functions
    list(SetData = SetData, GetData = GetData,
         SetInverse = SetInverse, GetInverse = GetInverse)
}


## Write a short comment describing this function
## Return the inverse of a matrix, computed or cached
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$GetInverse ()
    if (!is.null(m)) {
        message("Getting Cached Solve or Inverse")
        return (m)
    }
    origdata <- x$GetData()
    m <- solve(origdata)
    x$SetInverse(m)
    return(m)
}
