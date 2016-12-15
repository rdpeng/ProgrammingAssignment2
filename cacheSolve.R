
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