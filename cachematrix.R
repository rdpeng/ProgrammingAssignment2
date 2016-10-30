##These functions compute the inverse of a matrix and cache the value. 
##If the inverse has already been computed, it retreives the value from cache.
## makeCacheMatrix():takes a matrix object and creates a special 
## "matrix" object that can cache itself.

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y){
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) inverse <<- solve
      getinverse <- function() inverse
      list(set = set, get = get, 
           setinverse = setinverse, 
           getinverse = getinverse)
}

## cacheSovle(): takes a makeCacheMatrix() object and computes the inverse of the special"matrix" returned
## by makeCacheMatrix(). If it has already been calculted
## it retrieves the inverse form the cache.

cacheSolve <- function(x, ...) {
      inverse <- x$getinverse()
      if(!is.null(inverse)){
            message("getting cached data")
            return (inverse)
      }
      data <- x$get()
      inverse <- solve(data,...)
      x$setinverse(inverse)
      inverse
}
