## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix() and cacheSolve() are the 2 functions used to return Cached Matrix

## makeCachematrix() Function takes matrix as input and returns Functions List.


makeCacheMatrix <- function(x = matrix()) {
y = x[1]
            inverse <- NULL
            
            ## Set the value of the matrix inverse to be NULL

            set <- function(y){
                 
                    x <<- y
                     
                    inverse <<- NULL 
             
            }
            
            ## Get the value of the matrix which is initially set to NULL.
  
            get <- function() x
            
            ## Set the value of the inverse matrix.
             
            setinverse <- function(minverse) inverse <<- minverse
            
            ## Get the value of the inverse matrix.
             
            getinverse <- function() inverse
            
            ## Return a list of functions.
             
            list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
             

}


## Write a short comment describing this function

## cacheSolve()Function takes a matrix and calculates its inverse, if it matches with cached value
## then return the Cached Value as Result

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
		
		 inverse <- x$getinverse()
            
            ## if the inverse matrix matches cached value 
            
            if(!is.null(inverse)){
                                 
                    return(inverse)
                    
            }
            
            data <- x$get()
            
            ## Calculate inverse.
            
            inverse <- solve(data, ...)
            
            ## Set the inverse.
            
            x$setinverse(inverse)    
            
            inverse            
            
            
 }
