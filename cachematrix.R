
  makeCacheMatrix <- function(x = matrix()) {
   
    
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
    
    
}
    
    cacheSolve <- function(x, ...) {

      inv <- x$getinv()
      if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinve(inv)
      inv
    }
