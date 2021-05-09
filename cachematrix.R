## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

}

## Write a short comment describing this function
This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
