## Put comments here that give an overall description of what your
## functions do

## MakeCacheMatrix contains all functions to set and get the data and to cache and return the results

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


## cacheSolve first checks, if cached results exist and returns them. 
## If cached results exist, it gets the data, calculates results, caches them and returns them.

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
