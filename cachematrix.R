## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## it supports setting matrix, getting matrix, setting inverse and getting
## inverse

makeCacheMatrix <- function(x = matrix()) {
         mtrx <- NULL 
    
   
    
    set <- function(y) {
        x <<- y
        mtrx <<- NULL
    }
 
    get <- function() x
    
    setinverse <- function(inverse) mtrx <<- inverse
    
    getinverse <- function() mtrx
    
   
    list(setmatrix=set, getmatrix=get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cachesolve will retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
         mtrx <- x$getinverse()
   
    if(!is.null(mtrx)) {
        
        message("...getting cached data.")
        
        return(mtrx)
    }
    
   
    data <- x$getmatrix()
    
   
    mtrx <- solve(data)
    
   
    x$setinverse(mtrx)
    
   
    mtrx
       
}
