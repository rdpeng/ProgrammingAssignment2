## This function generates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  l <- NULL
  set <- function(y) {
    x <<- y
    l <<- NULL
  }
  get <- function() x
  setsolution <- function(solution) l <<- solution
  getsolution <- function() l
  list(set = set, get = get,
       setsolution = setsolution,
       getsolution = getsolution)
}

## This function gets the inverse of the special "matrix" returned by the function above.

cachesolution <- function(x, ...) {
  l <- x$getsolution()
  if(!is.null(l)) {
    message("loading inverse matrix")
    return(l)
  }
  data <- x$get()
  l <- solution(data, ...)
  x$setsolution(l)
  l
}
