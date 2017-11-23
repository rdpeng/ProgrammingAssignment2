#cache the inverse of a matrix.


#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() {
    x
  }

  setsolve <- function(solve) {
    i <<- solve
  }
  getsolve <- function() {
    i
  }
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

#This function computes the inverse of the special "matrix" returned 
#by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve 
#the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getsolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
  
  ## Return a matrix that is the inverse of 'x'
  i
}