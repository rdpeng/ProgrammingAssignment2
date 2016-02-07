## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                                   # setting starting values
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x                         # return data
    setsolve <- function(solve) m <<- solve     # caches result in m
    getsolve <- function() m                    # return cached result
    list(set = set, get = get,                  # return functions as list
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getsolve()                             # get cached results
  if(!is.null(m)) {                             # if cache is not empty
    message("getting chached data")             # return message
    return(m)                                   # and cached results
  }
  data <- x$get()                               # assign matrix to data
  solve <- solve(data)                          # solve matrix and assign result to solve
  x$setsolve(solve)                             # cache result
  solve                                         # return solve
}
