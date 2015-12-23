# Following are two functions to be used to 1) create a "matrix"
# that can cache its inverse (makeCacheMatrix); 2) compute the
# inverse of the matrix or retrieve the already calculated inverse 
# from the cache if it exists (cacheSolve).

# makeCacheMatrix will create a "matrix" which is a list 
# containing functions to a) set value of matrix; b) get value
# of matrix; c) set value of the inverse of the matrix; d) get 
# the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) i <<- solve
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

# cacheSolve will compute the inverse of the "matrix" created by
# makeCacheMatrix if the inverse has not yet been computed.

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i ## Return a matrix that is the inverse of 'x'
}


        
