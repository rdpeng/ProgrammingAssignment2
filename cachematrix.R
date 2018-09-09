## Due to time an computational constraints, It may be useful to cach the results of a function for ease or use later.
## function is designed to cach a matrix, And ecall the inverse of the matrix at a later stage.

## caching of matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## retreiving the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if (!is.null(i)) {
    message("retreving cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
