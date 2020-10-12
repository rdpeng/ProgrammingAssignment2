##The funcctions retrieve the inverse from the cache if it exists, if not it computes

##The first function, makeCacheMatrix creates a special “matrix”, which is really a list containing a function to:
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

##This function computes the inverse of the special “matrix” returned by makeCacheMatrix above
##If it exists in cache, it retrieves it



cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if (!is.null(i)) {
          message("retrieving cache")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}












