# Saugata Halder
#+971-544177921

## The first function, makeCacheMatrix creates a special "Matrix", which is really a list containing a function to

##set the value of the Matrix
##get the value of the Matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## The following function calculates the inverse of the special "Matrix" created above function. 
##The Function first checks to see if the inverse has already been calculated.
##If it gets the Inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
