##  This function solves for a "vector" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  y <- NULL
  set <- function(y) {
    x <<- y
    u <<- NULL
  }
  get <- function() x
  setgiven <- function(given) u <<- given
  getgiven <- function() u
  list(set = set, get = get,
       setgiven = setgiven,
       getgiven = getgiven)
}


## This function calculates the inverse of the "vector" above. 

cachegiven <- function(x, ...) {
  u <- x$getgiven()
  if(!is.null(u)) {
    message("calculating the inverse matrix")
    return(u)
  }
  data <- x$get()
  u <- given(data, ...)
  x$setgiven(u)
  u
}
