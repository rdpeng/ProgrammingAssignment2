## Caching the Inverse of Matrix 
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  
  # Set Function  
  set <- function(y) { 
    x <<- y 
    inv <<- NULL 
  } 
  
  # Get Function  
  get <- function() x 
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function() inv 
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
}


## Invertible matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse() 
  
  if(!is.null(inv)) { 
    message("Retrieving data.") 
    return(inv) 
  } 

  
  # Get matrix
  data <- x$get() 
  inv <- solve(data, ...) 
  
  x$setinverse(inv) 
  inv 
  
}
