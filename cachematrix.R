## Put comments here that give an overall description of what your
## functions do
## Programming assignment 2 Kevin Workman
## makceCacheMatrix accepts a matrix, caches the inverse of the matrix. 
## cacheSolve calculates the inverse of a matrix unless it's held in the cache, in which case it returns the cached version.

## Write a short comment describing this function
## Returns a list of functions to manipulate the cache of a calculated inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  minverse <- NULL
  set <- function(y){
        x <<- y
        minverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) minverse <<- inverse
  getInverse <- function() minverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## Will return the inverse of a cachable matrix.  Matrix must be run through makeCacheMatrix() before 
## cacheSolve will solve it
cacheSolve <- function(x, ...) {
  ##check the cache for minverse, return cached inverse
  minverse <-x$getInverse()
  if(!is.null(minverse)){
            message("getting cached data")
            return(minverse)
  }
  ## solve the uncached inverse
  data <- x$get()
  m <- solve(data)
  ## load the cache
  x$setInverse(m)
  m
}
