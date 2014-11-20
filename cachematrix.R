## First of all, a BIG shout out to Bill Hilton, who's posts in the
## discussion forums really helped explain this assignment in a way
## made it easy to understand. Thanks!
##
## Together, these two functions invert a matrix and cache that
## inverted matrix to be called upon later, avoiding unnecessary calculations.

## makeCacheMatrix creates an object of type 'list' that includes the matrix,
## the inverse of the matrix (or a NULL placeholder), a function that gets the
## the matrix, a function that sets the inverse matrix and a function that gets
## the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL    
     
     get <- function() 
          { x }   
     setinverse <- function(inv)  
          { m <<- inv }  
     getinverse <- function() 
          { m }
     
     list(get = get,                
          setinverse = setinverse,  
          getinverse = getinverse)       
}


## cacheSolve will look in the list created by makeCacheMatrix for the value of the inverted
## matrix. If the value is there, it returns that value and ends. If the value is not there,
## it will invert the matrix and use the setinverse function defined in makeCacheMatrix to 
## cache that value. Then, it will return the value of the inverted matrix. The next time 
## cacheSolve is ran, it will see that there is a value for the inverted matrix and not do the
## inversion calculation.

cacheSolve <- function(x, ...) {
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")  
          return(m)                       
     }
     data <- x$get()        
     m <- solve(data, ...)  
     x$setinverse(m)           
     m               
}
