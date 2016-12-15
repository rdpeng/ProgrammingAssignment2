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
