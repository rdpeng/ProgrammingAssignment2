# makeCacheMatrix: This function creates a special "matrix" object 
# that can cache its inverse.

# cacheSolve: This function computes the inverse of the 
# special "matrix" returned by makeCacheMatrix. 
# If the inverse has already been calculated 
# (and the matrix has not changed), then the inverse is retrieved 
# from the cache.

# We assume that the matrix supplied is always invertible.

# Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  y <- NULL  # matrix's inverse
  set <- function(d) {
    x <<- d
    y <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) y <<- inv
  getinverse <- function() y
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# computes the inverse of the special "matrix" 
# or retrieves it from the cache if this has been computed before
cacheSolve <- function(x, ...) {
  y <- x$getinverse()
  if(!is.null(y)) {
    message("getting cached data")
    return(y)
  }
  data <- x$get()
  y <- solve(data) 
  x$setinverse(y)
  y # the inverse of 'x'
}
