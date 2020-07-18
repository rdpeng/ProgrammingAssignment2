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
##Test matrix for checking
'm1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1
I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
I2
n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
n1
m1 %*% n1
n1 %*% m1
solve(m1)
solve(n1)
myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)
cacheSolve(myMatrix_object)'


