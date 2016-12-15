##this function creates a special matrix object that can cache its inverse

makeCacheMatrix <-function(x = matrix())
{
  inv <- NULL
  
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  #this is the cached object. The x value comes from the cachesolve call
  setInverse <- function(solve) inv <<- x
  getInverse <- function() inv
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

##this function computes the inverse if its not in the cache. 

cacheSolve <- function(x, ...)
{
  #this call gets the cached value, if it exists
  inv <- x$getInverse()
  
  if(!is.null(inv))
  {
    print("Getting cached data...")
    return(inv)
  }
  
  #if the value is not cached, then it is computed here
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inv)
  
  inverse
}