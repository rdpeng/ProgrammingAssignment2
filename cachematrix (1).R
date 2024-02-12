## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## the function makes a matrix object caching its own inverse for the input.  

makeCacheMatrix <- function(x = matrix()) {inv <- NULL
set <- function(y) {
  x <<- y
  inv <<- NULL
}
get <- function() x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv
list(set = set, get = get, setInverse = setInverse,getInverse = getInverse)}

## Write a short comment describing this function
## cacheSolve calculates the inverse of the matrix from makeCacheMatrix. If already inverse, it then takes the answer from cache. 

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
