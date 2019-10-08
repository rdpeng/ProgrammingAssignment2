##Matrix inversion is usually a costly computation and there are benefits
##to caching the inverse of a matrix rather than compute it repeatedly

##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
      I <- NULL
      ##setting the matrix x
      set <- function(y) {
            x <<- y
            I <<- NULL
      }
      ##getting the matrix x
      get <- function() x
      ##setting the inverse I
      setinverse <- function(inverse) I <<- inverse
      ##getting the inverse I
      getinverse <- function() I
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
      ##finding the cache
      I <- x$getinverse()
      if(!is.null(I)) {
            ##if the cached Inverse exists for x(i.e. x$getinverse() is not null) then print message and 
            ##return cached Inverse
            message("getting cached data")
            return(I)
      }
      ##otherwise find the find the inverse
      data <- x$get()
      I <- solve(data, ...)
      x$setinverse(I)
      I
}