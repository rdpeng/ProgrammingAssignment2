# This intent of this function is to create a matrix that can cache its inverse.
# Matrix inversion can be a costly computation. The two funcions below create an object that stores the matrix that 
# can cache its inverse.

makeCacheMatrix <- function(x = matrix){    ## This function create the matrix can can cache its inverse.
  m <- NULL # empty inverse of a matrix
  set <- function(y){   # (x ==y) to the parent enviroment <<-
    x <<- y
    m <<- NULL
  }
  get <- function() {x}  # gets the value of the matrix 
  setInverse <- function(inverse) m <<- inverse # sets the value of the matrix to the parent env
  getInverse <- function() {m} # gets the value of m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
# This function computers the inversre of the special matrix returned by the makeCachematrix. If the inverse has not been 
#calculated - then cacheSolve will inverse the cache.
cacheSolve <- function(x, ...) {
  # return a matrix that is an inverse of 'x'
  m <- x$getInverse() # getst the value of a an inverse matrix
  if(!is.null(m)){    ## ifthe matrix  exists then the inverse has been calculated.
    message("getting cached data")
    return(m)
  }
  data <-x$get() # gets the value of the matrix
  m <- solve(data, ...) # calculates the inverse
  x$setInverse(m)
  m # return the inverse of a matrix 
}