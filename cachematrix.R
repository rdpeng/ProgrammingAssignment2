## The following program describes a pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_x<<- inverse
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of a "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv_x <- x$getinverse()
  if(!is.null(inv_x)) {
    message("getting cached inverse matrix")
    return(inv_x)
  }else{
    inv_x <- solve(x$get())
    x$setinverse(inv_x)
    return(inv_x)
  }
}
