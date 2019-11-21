makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<-NULL
  }
  get <- function() x
  set.inv <- function(inv) m <<- inv
  get.inv <- function() m
  list(set = set, get = get, set.inv = set.inv, get.inv = get.inv)
}

cacheSolve <- function(x, ...) {
  m <- x$get.inv()
  if(!is.null(m)) {
    message("Getting cached data.")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set.inv(m)
  m
}
