## Put comments here that give an overall description of what your
## functions do

# The two functions work together to cache the inverse of a matrix


## Write a short comment describing this function

# This function creates a "special" matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set=set, 
       get=get, 
       setinv=setinv, 
       getinv=getinv)
}




## Write a short comment describing this function

# This function works out the inverse of the "special" matrix object returned from makeCacheMatrix above.
# If the inverse has not already been calculated and the matrix has not been changed, 
# then this function returns the inverse from the cache. 
# The assumption is made that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data") 
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
