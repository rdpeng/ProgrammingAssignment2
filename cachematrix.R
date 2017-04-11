##The purpose of these functions are to compute and cache the inverse of a 
##square matrix in order to save the time of unnecessary repeated computations

##This function creates a special "matrix" object that can cache its inverse by
##creating a list of functions to set and get the value of a matix and the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {                         ##sets the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x                          ##gets the value of the matrix
  setinverse <- function(solve) m <<- solve    ##sets the value of the matrix inverse
  getinverse <- function() m                   ##gets the value of the matrix inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##This function computes the inverse of the special "matrix" returned by the 
##makeCacheMatrix function. If the inverse has already been calculated the inverse 
##will be retrieved from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()                          ##uses getinverse to check for cached data
  if(!is.null(m)) {                            ##if the inverse of the matrix is
    message("getting cached data")             ##cached this message will print along
    return(m)                                  ##with the stored value of the inverse
  }
  data <- x$get()                              ##If there is no cached value for the
  m <- solve(data, ...)                        ##inverse; it is computed using the solve
  x$setinverse(m)                              ##function, then cached for future use
  m
}