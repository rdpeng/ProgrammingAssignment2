## Put comments here that give an overall description of what your
## functions do

## The MakeCacheMatrix function contains a special matrix object that can cache the inverse

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL                       
                                 
  
  get <- function() { x }        
  
  setinverse <- function(solve)  
  { m <<- solve }                
  
  getinverse <- function() { m } 
                                 
  
  list(get = get,                       
       setinverse = setinverse,  
       getinverse = getinverse)  
                                 
}


## CacheSolve checks to see if the inverse has been found yet and if so returns it. 
## If not it calculates it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'                                
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
