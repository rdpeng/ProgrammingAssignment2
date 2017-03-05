<<<<<<< HEAD
## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.
## it supports setting matrix, getting matrix, setting inverse and getting
## inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invers) inv <<- invers
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
=======
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Below are a pair of functions that are used to create a special object
##that stores a matrix and caches its inverse.
## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

##This functoin computes the inverse of the special 'matrix' created by
## makeCacheMatrix above. If the inverse has already been calculated
## then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
>>>>>>> origin/master
