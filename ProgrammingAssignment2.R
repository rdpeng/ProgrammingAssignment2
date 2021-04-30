makeCacheMatrix <- function(x = matrix() ) {
         
          #set/get matrix provided by user
          m <- NULL
          set <- function(y) {
                    x <<- y
                    m <<- NULL
          }
          get <- function() x
          setsolve <- function(solve) m <<- solve
          getsolve <- function() m
          list(set = set, get = get,
               setsolve = setsolve,
               getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
          m <- x$getsolve()
          if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
          }
          data <- x$get()
          m <- solve(data, ...)
          x$setsolve(m)
          m
}


# #example of how makeVector works
# mvec <- makeCacheMatrix()
# x <- matrix(1:4, nrow=2, ncol=2)
# mvec$set(x)
# mvec$get()
# mvec$getsolve()
# mvec$setsolve(x)
# mvec$getsolve()
# 
# #example of how cachemean works
# cacheSolve(mvec)
