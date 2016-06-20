## These two functions are used to find the inverse of a square matrix
## (which has an inverse), and cache the result to increase the speed
## of calculating the inverse of matrices.


## makeCacheMatrix() creates a special matrix that caches its inverse

## the argument 'x' is a square matrix that has an inverse

## the function returns a list of functions which become the inputs
## for cacheSolve()

## this function list will; set the matrix, get the matrix, set the inverse
## get the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve() returns the inverse matrix of the original matrix that was
## the argument in makeCashMatrix()

## the argument of 'x' is what's returned from the function makeCashMatrix()

## if() tests to see if the inverse of that matrix has been
## previously calculated
## if if() finds the inverse in cached data, this matrix is returned
## if if() doesn't find the inverse in cached data, the inverse is calculated
## finally, this 'new' inverse is set in the cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
