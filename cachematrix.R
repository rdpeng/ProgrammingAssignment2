## The first function 'makeCacheMatrix' creates a matrix object that can get and set its inverse in cache
## The 'cacheSolve' function first checks to see if the matrix is present in the cache to retrieve and if not, computes the inverse of the matrix created in the above function


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# This function returns the inverse of the given matrix. It first checks to see
# if the inverse has already been computed. If so, it gets the result fromt he cache 
# and skips the computation. If not, it computes the inverse, sets the value in the 
# cache via setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}