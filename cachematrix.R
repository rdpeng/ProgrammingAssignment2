## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
         mtrx <- NULL #initialize mtrx as NULL, it will hold the value of matrix inverse
    
   
    
    set <- function(y) {  #set function is defined to assign new value of matrix in parent environment
        x <<- y              
        mtrx <<- NULL  #if there is a new matrix , re assign mtrx to NULL
    }
 
    get <- function() x   #returns the value of matrix argument
    
    setinverse <- function(inverse) mtrx <<- inverse  #assigns value of inverse in parent environment
    
    getinverse <- function() mtrx  #gets value of inverse
    
   
    list(setmatrix=set, getmatrix=get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix is unchanged), then cachesolve will retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
         mtrx <- x$getinverse()  #return the matrix that is inverse of 'x'
   
    if(!is.null(mtrx)) {
        
        message("...getting cached data.")
        
        return(mtrx)
    }
    
   
    data <- x$getmatrix()
    
   
    mtrx <- solve(data)
    
   
    x$setinverse(mtrx)
    
   
    mtrx
       
}
