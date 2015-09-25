## The function below cache potentially time-consuming computations

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
              invs <- NULL
              set <- function(b) {
                x <<- b
                invs <<- NULL
              }
              get <- function() x
              setsolve <- function(solve) invs <<- solve
              getsolve <- function() invs
              list(set = set, get = get,
                   setsolve = setsolve,
                   getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        invs <- x$getsolve()
        if(!is.null(invs)) {
          message("getting cached data")
          return(invs)
        }
        data <- x$get()
        invs <- solve(data, ...)
        x$setsolve(invs)
        invs
}
