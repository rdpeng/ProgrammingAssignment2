# makeCacheMatrix creates a special "vector", which is a list containing a function to
# 1) get the values of the matrix
# 2) set the values of the matrix
# 3) get the inverse of the matrix
# 4) set/calculate the inverse matrix
makeCacheMatrix <- function(x = matrix())
{
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# The cacheSolve function calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse matrix from the cache and skips the computation. 
# Otherwise, it calculates the inverse matrix.
cacheSolve <- function(x, ...)
{
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  message("calculating inverse")
  x$setInverse(m)
  m
}
