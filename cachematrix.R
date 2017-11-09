#Programming Assignment 2

#-------------------------caching the mean of a vector------------------------------
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}
x<-c(2,3,4,5,6,7,8)
makeVector(x)


cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

#------------------------------------------------------------------------------------
#-------------------------caching the inverse of a matrix----------------------------

#task: create the following functions:

#makeCacheMatrix: This function creates a special "matrix" object that 
#can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. 
#If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should 
#retrieve the inverse from the cache.

#using solve function
#Computing the inverse of a square matrix can be done with the solve 
#function in R. For example, if X is a square invertible matrix, then 
#solve(X) returns its inverse.
#For this assignment, assume that the matrix supplied is always invertible.

#makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#----------------------------------------------------------
#cacheSolve

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}


