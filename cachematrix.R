## These functions take a matrix and its inverse and store both in the  
## environment. If matrix is unchanged, cached inverse is returned. If matrix 
## is changed, new inverse is calculated, stored and returned.

## makeCacheMatrix() takes a matrix as argument. 
## Note: There is (yet) no check that the matrix is square and no NAs
## The rest is basically the same as the example provided in the example.
## I changed a few names for clarity only
## The function returns a list of 4 functions: get, set, setinv & getinv
## 'inv' is the inverse matrix 'cached'

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## Same as cachemean() provided in the example, few names changed.
## If there is a inverse stored related to x, it is returned
## If not, the inverse is calculated, stored and returned

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("Getting cached data...")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
