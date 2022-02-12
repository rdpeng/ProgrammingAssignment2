
## Functions that cache the matrix inverse.
## Matrix object caches its inverse

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
set <- function(y) {

  x <<- y

  m <<- NULL
}
get <- function () x

setInverse <- function(solveMatrix)
  m <<-solveMatrix
getInverse <- function() m
list(set = set,
     get=get,
     setInverse = setInverse,
     getInverse = getInverse)


}


## The inversion computation of matrix returned by makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'xx'

  m <-x$getInverse()
  if (!is.null(m)){
    message("Getting cached data")
    return (m)
  }
  data <-x$get()
  m <-solve(data)
  x$setInverse(m)
  m
}
