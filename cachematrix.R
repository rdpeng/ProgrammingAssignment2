##  These functions are for programming assignment 3 for R-programming Coursera
##    
## 
## the 'makeCacheMatrix' function creates a list from a matrix input
## 

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x ##typing x$get will print the matrix
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m ##typing x$getinverse will print the matrix inverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      ## if this function has been run before, will get the stored values, rather than recalculate
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

