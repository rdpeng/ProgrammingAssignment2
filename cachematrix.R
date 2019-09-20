## The function creates a special "matrix" object that cache its inverse



makeCacheMatrix <- function(x = matrix()) {
        m <- NULL .   set <- function(matrix){ x <- matrix
                                              
         m <- NULL      
      }
        
        get <- function()x
        setinverse<- function(inverse)
                m<<- inverse
        
        getinverse <- function()m
        
        
        
        
## The function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
       
inv <- x$getinverse()
if (!is .null(inv)){ message ("cached data)
}
