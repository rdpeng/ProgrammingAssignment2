## Here two functions 'makeCacheMatrix()' and 'cacheSolve()' functions are implemented.
## 'makeCachematrix()' takes a matrix as a argument and return a list of functions.
## 'cacheSolve()' takes a matrix object and calculate its inverse and if already 
## existed in cache then simply return that.
## Here 'solve()' functions is used to calculate inverse, which takes square matrix
## and if it is invertible then return inverse. So, square invertible matrix is 
## given as a input matrix.

## 'makeCachematrix()' takes a matrix as a argument and return a list of functions 
## named 'set()', 'get()', 'setinverse()' and 'getinverse()'.

makeCacheMatrix <- function(x = matrix()) {
    
            ## intially inverse matrix is NULL.
  
            inverse <- NULL
            
            ## Set the value of matrix.
             
            set <- function(y){
                 
                    x <<- y
                     
                    inverse <<- NULL 
             
            }
            
            ## Get the value of matrix.
  
            get <- function() x
            
            ## Set the value of inverse matrix.
             
            setinverse <- function(minverse) inverse <<- minverse
            
            ## Get the value of inverse matrix.
             
            getinverse <- function() inverse
            
            ## Return a list of functions.
             
            list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
             
}


## 'cacheSolve()' takes a matrix object and calculate its inverse and if already 
## existed in cache then simply return that. solve() is used to calculate matrix
## inverse and return the inverse matrix.

cacheSolve <- function(x, ...) {
    
            ## Get inverse matrix .
  
            inverse <- x$getinverse()
            
            ## if inverse matrix is previously computed then return that.
            
            if(!is.null(inverse)){
                
                    message("Getting cached data")
                    
                    return(inverse)
                    
            }
            
            ## Get the matrix.
            
            data <- x$get()
            
            ## Calculate the inverse.
            
            inverse <- solve(data, ...)
            
            ## Set the inverse.
            
            x$setinverse(inverse)
            
            ## Return a matrix that is the inverse of 'x'.
            
            inverse            
            
            
}
