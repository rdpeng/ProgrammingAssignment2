## The function creates a special "matrix" object that cache its inverse.
## In the (1) function,we create a  special"matrix" object which cache its inverse.

 makeCacheMatrix <- function(x = matrix()) {
        in <- NULL 
         set <- function(y){ x <<-y
                                              
         in <- NULL      
      }
        
        get <- function()x
        setInverse<- function(inverse)
                in<<- inverse
        
        getInverse <- function()in
         
         list(set = set, get = get,  
             setInverse = setInverse, getInverse = getInverse) 
              
      }  
        
                
## (2)function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
                     
in <- x$getInverse()
if (!is .null(in)){
        message ("cached data")
        return(in)
}
        data <-x$get()
        in <- solve(data)
        x$setInverse(in)
        in
}
