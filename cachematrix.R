## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   ## Initialize inverse 
     i <- NULL 
  
     ## Set the matrix 
     set <- function( matrix ) { 
             m <<- matrix 
             i <<- NULL 
     } 
 
     ## Get the matrix 
     get <- function() { 
     	## Return the matrix 
     	m 
     } 
  
     ## Set the inverse of the matrix 
     setInverse <- function(inverse) { 
         i <<- inverse 
     } 
 
     ## Get the inverse of the matrix 
     getInverse <- function() { 
         ## Return the inverse matrix
         solve(m) 
     } 
 
     ## Return a list of the methods 
     list(set = set, get = get, 
          setInverse = setInverse, 
          getInverse = getInverse) 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse() 
 
        ## Return the inverse if its already set 
        if( !is.null(m) ) { 
             message("getting cached data") 
             return(m) 
        } 
  
        ## Get the matrix from the object 
        data <- x$get() 
  
        ## Get the inverse using matrix multiplication 
        m <- solve(data) %*% data 
  
         ## Set the inverse  
        x$setInverse(m) 
  
     ## Return the matrix 
     m 
}
