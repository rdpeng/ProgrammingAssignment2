## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                             ## initialize inv as NULL; will hold value of matrix inverse 
    set <- function(y) {                    ## define the set function to assign new 
        x <<- y                             ## value of matrix in parent environment
        inv <<- NULL                        ## if there is a new matrix, reset inv to NULL
    }
    get <- function() x                     ## define the get fucntion - returns value of the matrix argument
    
    setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
    getinverse <- function() inv                     ## gets the value of inv where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer 
                                                                                  ## to the functions with the $ operator

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
       #get the value of the invertible matrix from the makeCacheMatrix function	
          invMatrix <- x$getInverse()	
        if(!is.null(invMatrix)) {                       #if inverse matrix is not NULL	
          message("Getting Cached Invertible Matrix")   #Type message: Getting Cached Invertible Matrix 	
          return(invMatrix)                             #return the invertible matrix	
        }	
          	
#if value of the invertible matrix is NULL then  	
        MatrixData <- x$getMatrix()                     #get the original Matrix Data 	
        invMatrix <- solve(MatrixData, ...)             #use solve function to inverse the matrix	
        x$setInverse(invMatrix)                         #set the invertible matrix 	
       return(invMatrix)                               #return the invertible matrix
}
