## To be able to reuse the calculated inverse of a matrix, these functions
## create a special matrix with a cache for its inverse

## This function 'converts' the matrix that is passed as an argument into
## a matrix with cache for its inverse with the help of lexical scoping,
## i.e. returning a list of functions
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


## This function returns the inverse of a matrix created by
## the function makeCacheMatrix() - if the inverse for this
## particular matrix has already been calculated, the function uses the
## cached value, otherwise the inverse is computed and stored for future access
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setinverse(inverse)
    inverse
}