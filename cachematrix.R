## Put comments here that give an overall description of what your
## functions do
 ## cashing the inverse of a matrix
## Write a short comment describing this function
 ## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
     set <- function(y){               
            x <<- y               
            inv <<- NULL      
     }       
     get <- function() {            
             x       
     }       
     setInverse <- function(inverse){             
                   inv <<- inverse       
     }       
     getInverse <- function(){            
                   inv       
     }       
     list(set = set, get = get,             
          setInverse = setInverse,            
          getInverse = getInverse)
}


## Write a short comment describing this function
 ## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv <- x$getInverse()       
         if(!is.null(inv)){               
                message("getting cached data")              
                return(inv)       
         }       
         data <- x$get()       
         inv <- solve(data, ...)       
         x$setInverse(inv)       
         inv
}
