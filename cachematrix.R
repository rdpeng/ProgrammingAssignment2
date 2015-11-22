## The purpose of below functions is to store the matrix with its inverse.
## So you don't need to call function solve() every time you need
## the matrix inverse. Function makeCacheMatrix transforms a simple matrix
## to the one, that can store its inverse. Function cacheSolve solves 
## the matrix inverse for a 1st time you call it and puts it in a cache.  
## Next time you call cacheSolve for the same matrix it simply gets inverse 
## from the cache rather than try to calculate it again.

## makeCacheMatrix takes a simple matrix as an input and returns
## the matrix that can store its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  setMatrix <- function(y) {
    x <<- y
    # once we set/change matrix we need to calculate its inverse
    inv <<- NULL 
  }
  getMatrix <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  # return a list of a functions that can set or return values of a matrix
  # and its inverse
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## cacheSolve takes modified by the above function matrix and returns its
## inverse. 1st time it calculates the inverse. Next times for this matrix
## function gets stored value from the cache.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

  m <- x$getInverse()
  
  # if inverse of this matrix was stored return from the cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # otherwise calculate inverse and put it to the cache for next uses
  data <- x$getMatrix()
  m <- solve(data, ...)
  x$setInverse(m)
  
  # return calculated inverse
  m
}
