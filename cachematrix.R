## These functions are designed to create a matrix and
## cache its inverse, then retreive the inverse from
## the cache rather than re-calculating the inverse.

## This function creates a matrix and caches the
## inverse. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL ## This creates an object m which is used later in the code
    set <- function(y) {
        x <<- y ## this sets the value of x as y and stores it in a function called set()
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function calculates the inverse of the matrix
## which was returned by makeCacheMatrix(). If the matrix
## has not changed and the inverse has already been
## calculated then cachesolve will retrieve the inverse
## from the cache rather than re-calculating. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getsolve()
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      } ## here if the object m is NULL then the matrix inverse has already been calculated and will return the cached inverse.
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m ## here if m is not set as NULL then the function will calculate the inverse
}
