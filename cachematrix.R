## The function makes a special "vector" that cache its inverse

makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  v <- NULL
  set <- function(y) {
    x <<- y
    v <<- NULL
  }
  get <- function() x
  setcompute <- function(compute) v <<- compute
  getcompute <- function() v
  list(set = set, get = get,
       setcompute = setcompute,
       getcompute = getcompute)
}


## This function solves for the inverse special vector returned by the above function.

cachecompute <- function(x, ...) {
  v <- x$getcompute()
  if(!is.null(v)) {
    message("the inversed matrix was computed again")
    return(v)
  }
  data <- x$get()
  v <- compute(data, ...)
  x$setcompute(v)
  v
}
