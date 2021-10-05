## Put comments here that give an overall description of what your
## functions do
##Matrix inversion is usually a costly computation and there may be some benefit to caching
##the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion
##that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.


makeCacheMatrix <- function(x = matrix()) {
   ## Write a short comment describing this function
  #This function creates a special "matrix" object that can cache its inverse.
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
