## This programm is the second programming assignment for the online course R programming on coursera by user:haroldyin1024

  
## This function will Cache the Inverse of a Matrix


##This function creat a matrix to chache its inverse
      
    makeCacheMatrix <- function(x = matrix()) { 
          inv <- NULL                           
          set <- function(y) {                  
                x <<- y                         
                inv <<- NULL                   
            }
          get <- function() x                  
          
            setinverse <- function(inverse) inv <<- inverse  
            getinverse <- function() inv                     
            list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
                                                                                          
      }
      
      
        ##This function computes the matrix inverse above, the function will return the results from cache if it has already
        ##been computed and not changed
        
        cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
            inv <- x$getinverse()
            if(!is.null(inv)) {
                   message("getting cached data")
                   return(inv)
               }
             data <- x$get()
             inv <- solve(data, ...)
             x$setinverse(inv)
             inv
        }
        