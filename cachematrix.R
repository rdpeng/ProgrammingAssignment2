## This set of functions will be able to cache the inverse of a matrix
## given, calculating the inverse the first time it sees the matrix 
## and giving the cache information instead whenever this computation has to be 
## repeated, if the matrix hasn't changed.

## This function will cache the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This will return the cache matrix if this one already exists or it will 
## return the inverse matrix if it's the first time it sees it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
