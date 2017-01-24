## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # inv set to null
  inv <- NULL
  
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## getter/setter for matrix inverse
  getinv <- function() inv
  setinv <- function(inverse) inv <<- inverse
  
  ## return list of functions for matrix
  list(get=get, set=set, getinv=getinv, setinv=setinv)
}


cacheSolve <- function(x, ...) {
  inv <- x$getinv()

  ## return cached matrix inverse if it's been already computed
  ## message to show when the Inverse has alrady been calculated
    if (!is.null(inv)) {
   
    message("Inverse is already calculated")
    return(inv)}        
  
  m <- x$get()
     
  inv <- solve(m, ...)
  
  ## look for inverse
  x$setinv(inv)
  
  ## return inverse 
  return(inv)
}
