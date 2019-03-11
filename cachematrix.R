## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
