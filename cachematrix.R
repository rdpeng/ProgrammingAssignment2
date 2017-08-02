## Set and get vector; set and get the inverse.

 makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
         x <<- y
         m <<- NULL
     }
     get <- function() x
     setinv <- function(inv) m <<- inv
     getinv <- function() m
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
 }

##get the inverse 

cacheSolve <- function(x, ...) {
     m <- x$getinv()
     if(!is.null(m)) {
         message("getting cached data")
         return(m)
    }
     data <- x$get()
     m <- solve(data, ...)
     x$setinv(m)
     m
 }
