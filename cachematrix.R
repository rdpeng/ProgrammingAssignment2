## The function below receives a inversibel matrix, calculates its inverse and keeps the
## solution in cache memory

##  In fact it is divided in two steps. The firt step calculates and keeps the solution in cache
## The second step gets back the solution in cache, if available, or otherwise calculates the result  
makeCacheMatrix <- function(x = matrix()) {
  
    inverse <- NULL
    set <- function(y) {
    x <<- y
    inverse <<- NULL
    }
      get <- function() x
      setsolve <- function(solve) inverse <<- solve
      getsolve <- function() inverse
      list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## This is the second step

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getsolve()
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
      data <- x$get()
      inverse <- solve(data, ...)
      x$setsolve(inverse)
      inverse
}
