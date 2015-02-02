## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    # Inverse Matrix num of cols and rows should be the same
    if (ncol(m) == nrow(m)) {
      #Square Matrix
      x <<- y
      m <<- NULL  
    }else {
      m <- NULL
    }
    
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
      message("getting solve cached data")
      return(m)
    }
    else{
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
