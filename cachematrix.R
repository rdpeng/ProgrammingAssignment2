## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
m = NULL
   
   ## function to set matrix
   set = function( matrix ) {
      x <<- matrix
      m <<- NULL
   }
   
   ## function to get matrix
   get = function() {
      ## Return the matrix
      x
   }
   
   ## function to set the inverse of the matrix
   setInverse = function(inverse) {
      m <<- inverse
   }
   
   ## function to get the inverse of the matrix
   getInverse = function() {
      ## Return the inverse property
      m
   }
   
   ## Return a list of the functions
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
   
   ## Check if inverse is already set, if true it returns the inverse
   if( !is.null(m) ) {
      message("getting cached data")
      return(m)
   }
   
   ## Get the matrix from our object
   data = x$get()
   
   ## Calculate the inverse using matrix multiplication
   m = solve(data) %*% data
   
   ## Set the inverse to the object
   x$setInverse(m)
        m
   
   
   
}

