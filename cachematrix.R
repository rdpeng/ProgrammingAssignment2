## This function creates a special "matrix" object that can cache its inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL

}
        get <- function() x
        setinverse <- function(invers) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
 }



## x is initialized as a function argument,m is set to NULL, initializing it as an object 
##           within the makeVector() environment to be used by later code in the function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
      if(!is.null(m)) {
           message("getting cached data")
           return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
