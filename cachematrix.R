## Put comments here that give an overall description of what your
## functions do
## This are functions to cache teh inverse of a matrix. Because the computation 
## of Matrix inversion is very costly, it is a cost benefit to cash it after
## compute


## Write a short comment describing this function
## makeCacheMatrix is a function to create a special "matrix" object
## the object can cashe a inverse matrix, and has a list of functions
## to get and set the inversed matrix



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<-NULL
  }
  
  get <- function() x
  
  setMatrix <- function(invert) m <<- invert
  getMatrix <- function() m
  
  list(set = set, get = get, setMatrix = setMatrix,
       getMatrix = getMatrix)
  
  
}



## Write a short comment describing this function
## cahcheSolve is a function computes teh inverse of the special "matrix" 
## returned by makeCacheMatrix. if the inverse has been calcuated, it just retrieve
## from the cache



cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  orig_m <- x$get()
  m <- solve(orig_m)
  x$setMatrix(m)
  return(m)
  
  
}
