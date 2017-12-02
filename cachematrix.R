## ## Function creates a cache Matrix

## x: a square invertible matrix
  ## return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is used as the input to cacheSolve()
  
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# Gets the cached data if already available else create new cache matrix from above function
## x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  # if the inverse has already been calculated
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # otherwise, calculates the inverse 
  data <- x$get()
  i <- solve(data, ...)
  # sets the value of the inverse in cache
  x$setinverse(i)
  i
}
