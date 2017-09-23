## makeCacheMatrix and cacheSolve together create a cache of a matrix and its
## inverse (once the inverse is computed for the first time). Each subsequent
## call to cacheSolve for the same matrix will produce the cached copy instead
## of a call to Solve to compute the inverse. This saves computational effort
## for the matrix inversion if it is needed more than once.

## makeCacheMatrix stores an input matrix in the calling environment and sets
## its inverse (m) to NULL. Once cacheSolve is invoked, m holds the inverse in
## the calling environment.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)

}

## cacheSolve first checks the status of the inverse matrix m in the global
## environment. If m is NULL, solve is called to find the inverse and store it
## in the global environment. If m has been found previously and the input
## matrix has not changed, then the cached inverse matrix is returned.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}