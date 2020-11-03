## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) {
          iv <- NULL
          set <- function(y) {
            m <<- y
            iv <<- NULL  
          }
          get <- function() m
          setinv <- function(inv) iv <<- solve
          getinv <- function() iv
          list(set = set, get = get,
               setinv = setinv,
               getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
            iv <- x$getinv()
            if(!is.null(iv)) {
              message("getting cached data")
              iv
            }
            data <- x$get()
            iv <- solve(data, ...)
            x$setinv(iv)
            iv
}
