## The two functions provided below cache the inverse of a matrix

## Caching the inverse of the matrix through the special function

          makeCacheMatrix <- function(x = matrix()) {
          i <- NULL
          
          ##Setting up the matrix
          set <-function(matrix) {
          x <- matrix
          i <- NULL
          }
          
          ##Getting the matrix 
          get <- function() { 
          x
          }
   
          ##Setting inverse of the matrix
          setinverse <- function(inverse) {
          i <<- inverse
          }
          
          ##Getting inverse of the matrix
          getinverse <- solve(x) 
          
            i
    
          
          
          list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
         
          }

## Function for retrieving the inverse from the cache
          cacheSolve <- function(x, ...) {

        ## Inverse of matrix x 
             i <- x$getinverse                      
             if(!is.null(i)) {
             message("getting cached inverse")
             return(i)
             }
             data <- x$get()
             i <- solve(data)
             x$setinverse (i)
             i
          }
         
          
