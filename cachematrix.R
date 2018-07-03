## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

    makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL 
    setMatrix <- function(y) { 
    x <<- y 
    invMatrix <<- NULL 
} 
    getMatrix <- function() x                              #get the value of the Matrix 
    setInverse <- function(inverse) invMatrix <<- inverse  #set the value of the invertible matrix 
    getInverse <- function() invMatrix                     #get the value of the invertible matrix 
    list(setMatrix = setMatrix, getMatrix = getMatrix, 
    setInverse = setInverse, getInverse = getInverse) 
}


## Write a short comment describing this function
    
    cacheSolve <- function(x, ...) {
    invMatrix <- x$getInverse() 
    if(!is.null(invMatrix)) {                       #if inverse matrix is not NULL 
    return(invMatrix)                             #return the invertible matrix 
} 
    MatrixData <- x$getMatrix()                     #get the original Matrix Data  
    invMatrix <- solve(MatrixData, ...)             #use solve function to inverse the matrix 
    x$setInverse(invMatrix)                         #set the invertible matrix  
    return(invMatrix)                               #return the invertible matrix 
    ## Return a matrix that is the inverse of 'x' 
}
