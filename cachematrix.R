# makeChacheMatrix returns a list of functions to: set matrix, get matrix, set calculated inverse, get calculated inverse.
# cacheSolve returns the cached inverse if it exists. 

## Get inverse of a matrix and cache it. return a list object.

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL # set cachedInverse to NULL
  set <- function(y) { # define a function to set the matrix, x, to a new matrix, y. Reset cachedInverse to NULL
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) cachedInverse <<- solve # sets cachedInverse, to a new matrix, solve.
  getInverse <- function() cachedInverse # returns the inverse, cachedInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Obtain the inverse of a matrix if it is in cache. 

cacheSolve <- function(x=matrix(), ...) {
  cachedInverse<- x$getInverse()
  if(!is.null(cachedInverse)){      #if in cache, return the cached matrix
    message("getting cached data")
    return(cachedInverse)
  }
  matrix<-x$get()                   # otherwise, obtain the inverse of the matrix.
  cachedInverse<-solve(matrix, ...)
  x$setInverse(cachedInverse)
  cachedInverse
}