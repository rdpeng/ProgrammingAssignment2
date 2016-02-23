#The following two functions takes the inverse of the matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
    set <- function(y) {
      x <<- y
      n <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) n <<- solve
    getinverse <- function() n
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
     n <- x$getinverse()
    if(!is.null(n)) {
      message("getting cached matrix")
      return(n)
    }
    data <- x$get()
    n<- solve(data, ...)
    x$setinverse(n)
    n
  
}
