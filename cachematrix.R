

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
     x <<- y
     s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list( get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}



cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if (!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
  
}

#x <- makeCacheMatrix(matrix(1:4, 2, 2))
#cacheSolve(x)
