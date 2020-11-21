#
# The scope of both functions is to calculate the inverse just once 
# (not each time you need it!). The time consuming calculations
# are done each time the matrix changes. If there is no change,
# the already calculated inverse is taken from the CACHE.
# 
# MakeCacheMatrix creates a list, a special vector that contains
# a function to set the value of a matrix in cache (outside the
# current environment) and its inverse (set and setinverse); and 
# get the current value of the matrix and its inverse from cache
# (get and getinverse). Initially the inverse is NULL.
#
# 
makeCacheMatrix <- function(x) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#
# If the inverse is NULL,the function cacheSolve calculates the
# inverse using solve and put it in cache. If it is already
# calculated, the function gets it from cache and returns the inverse. 
#
cacheSolve <- function(x) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
#
#

