## The following function creates a list of functions that 
##stores the original matrix, one that returns the cached 
##matrix (if it exists), and one that sets the cached matrix 
##(if it exists), and a function that describes if an inverse is stored
makeCacheMatrix <- function(x = numeric()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinv <- function(inv) i<<-inv
     getinv <- function() i
     list(set = set , get = get, setinv = setinv, getinv = getinv)
}

## This matrix will solve for the inverse(if it does not exist),
## and stores it, or returns the cached inverse. 

cacheSolve <- function(x, ...) {
     i <- x$getinv()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data)
     x$setinv(i)
     i
}
