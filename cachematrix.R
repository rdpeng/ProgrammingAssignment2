## makeCacheMatrix function takes inversible matrix as input 
## creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y) 
  {
    
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix 
##  cachesolve returns the inverse of original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) 
{
  inv <- x$getinverse()
  
  # if the inverse has already been calculated
  
  if(!is.null(inv)) 
  {
    message("getting cached inverse matrix")
    
    # retrieve it from the cache and skips the computation. 
    
    return(inv)
  }
  #calculate the inverse 
  
  data <- x$get()
  
  inv <- solve(data, ...)
  # sets the value of the inverse in the cache via the setinverse function.
  
  x$setinverse(inv)
  
  
  inv ;
}
