## Cache the inverse of the matrix
## makeCacheMatrix fucntion creates a special matrix that can Cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
}
get <- function() x
setInverse <- function(inverse) invs <<- inverse
getInverse <- function(inverse) invs
list(set=set,
     get=get,
     setInverse=setInverse,
     getInverse=getInverse)
}

## CacheSolve calculates the inverse of the matrix made by makeCacheMatrix  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invs <- x$getInverse()
        if (!is.null(invs)) {
          message("getting the Cached data")
          return(invs)
        }
        matrx <- x$get()
        invs <- solve(matrx, ...)
        x$setInverse(invs)
        invs
  }
