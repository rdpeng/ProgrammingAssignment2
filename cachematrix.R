## Two functions that work together to expedite computation of inverting matrices.

## First function makes special object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ## where the magic happens ~*~*~
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # retrieve value of x
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  ## second magic part
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks to see if inverse of matrix from first function cached
## If not, calculates inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
