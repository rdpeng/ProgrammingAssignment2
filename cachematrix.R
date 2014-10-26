##Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix 
##rather than compute it repeatedly. 
##We try to write here a pair of functions that cache the inverse of a matrix.

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  return x
}


##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
##we assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  return solve(x)
}

## Example: Caching the Mean of a Vector

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

##

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
