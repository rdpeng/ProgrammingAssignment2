## Create a cached matrix using scoping rules which also allows user to also cache its inverse


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      x_inv <- NULL
      set <- function(y) {
            x <<- y
            x_inv <<- NULL
      }
      get <- function()
            x
      setinv <- function(inv)
            x_inv <<- inv
      getinv <- function()
            x_inv
      list(
            set = set, get = get,
            setinv = setinv,
            getinv = getinv
      )
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##             If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
##             should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      mtrx <- x$get()
      inv <- solve(mtrx)
      x$setinv(inv)
      inv
}
