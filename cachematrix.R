
## this function creates a special matrix which output a list of the get & set for the matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
      set <- function(y) {
              x <<- y
              m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)

}


## this function returns the inverse of a matrix either via the cached vlaue or, if this isn't available, calculating the inverse  

cacheSolve <- function(x, ...) {
     
        i <- x$getinverse()
      if(!is.null(i)) {
              message("getting cached data")
              return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i

}
