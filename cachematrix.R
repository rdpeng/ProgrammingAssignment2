## Setting the special matrix and caching its inverse


makeCacheMatrix <- function(x = matrix()) {
  ins <- NULL
  set <- function(y) {
    x <<- y
    ins <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) ins <<- inverse
  getinverse <- function() ins
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## computing the inverse of the special matrix

cacheSolve <- function(x, ...) {
  ins <- x$getinverse()
  if (!is.null(ins)) {
    message("getting cached data")
    return(ins)
  }
  data <- x$get()
  ins <- solve(data, ...)
  x$setinverse(ins)
  ins
}
