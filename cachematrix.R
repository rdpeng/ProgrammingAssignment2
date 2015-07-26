## These functions work together to create a matrix like 'object' and store that
## matrix and it's associated inverse in the memory cache allowing them to be 
## recalled quickly and cheaply when necessary

## this function creates an object like variable out of a matrix input
## the structure of this object allows it to store the matrix in the cache
## along with it's associated inv. This is adantageous because calculating the
## inverse of a matrix can be very computationaly taxing and this allows it to 
## be recalled from the cache, saving time.

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


## This function finds the inverse of a matrix. It works in conjunction with
## makeCacheMatrix. This function takes the output of the makeCacheMatrix
## function and finds the inverse of the matrix. It then sets the inverse 
## 'object' of the matrix to the inverse it has solved for

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$setinv(m)
      m
}
