#This function receives a matrix as a parameter and will calculate its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  # get the matrix
  get <- function() x
  # set the matrix
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  # returning of methods
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# inverse returned from cache
cacheSolve <- function(x, ...) {
  # matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data...")
          return(i)
  }
  # get the matrix
  data <- x$get()
  # Calculate the inverse using matrix multiplication
  i <- solve(data, ...)
  x$setinverse(i)
  # return of result matrix  
  i
}
