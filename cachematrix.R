## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
  ma <- NULL 
  set <- function(y) { 
    x <<- y 
    ma <<- NULL 
  }
  get <- function() x 
  setinvert <- function(inverse) ma <<- inverse
  getinvert <- function() ma 
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}

## Write a short comment describing this function


cacheSolve <- function(x, ...) {
  ma <- x$getinv() 
  if(!is.null(ma)) { 
    message("getting cached data")
    return(ma) 
  } 
  ma <- x$get() 
  ma <- solve(dataalm, ...) 
  x$setinvert(ma) 
  return ma 
}
