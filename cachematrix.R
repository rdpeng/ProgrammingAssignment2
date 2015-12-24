## In combination makecachematrix and cachesolve will create an inverse of a 
## square, invertible matrix provided. The inverted matrix will be stored in 
## an alterante environment to protect it from loacl changes and to reduce the
##  omputing cycles needed to to recreate it.

## The function makecachematrix will accept a square, invertable matrix. 
## It will first store the matrix in an alternative environment to protect
## it from changes in the local environment AND make it available to access 
## from other environments. The function returns a list that contains functions. 
## Among them, 'setinverse' will use the 'solve()' function to create the inverse,
## and store it in the cache. 'getinverse' will extract the inverse information 
## from the stored cache.

makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL
    
    set <- function(y) {
      
            x <<- y
            
            inv <<- NULL
            
            } ## end function(y)
    
    get <- function() x
    
    setinverse <- function(solve) inv <<- solve ## will invert the matrix and chache it
    getinverse <- function() inv ## will return the value in 'inv'
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) ## returns a list containing functions

} ## end makecachematrix


## The function cachesolver will first test to see if the inverse of the
## matrix given has been "solved". If it has (!is.null(inverse) = FALSE), 
## then the IF statement will NOT execute. The return statement will return 
## 'inv' and exit the function. If the inverse is NOT stored 
## in the variable 'inv', then the function will create and store the inverse 
## matrix using the function 'setinvers(x)'. 

cacheSolve <- function(x, ...) {
        ## If the inverse exists, Return a matrix that is the inverse of 'x'
  
      inv  <- x$getinverse()
      
      if(!is.null(inv)) { ## test for the inverse
        
        message("getting cached matrix")
        
        return(inv)
        
      } ##end if
      
      ##if the inverse matrix is not cached, find the inverse and then return.
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
      
} ## end cachesolve
