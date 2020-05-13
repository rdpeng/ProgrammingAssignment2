# Final Version
## *makeCacheMatrix*: This function creates a special "matrix" object that can cache its inverse
## The output of the makeCacheMatrix function caches the inverse of a matrix (x)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<-y
                inv <<- NULL
}

        
## inverses the matrix
get <- function() x
setinv <- function(inverse) inv <<- inverse
getinv <- function() inv
        
## print the output of the inverse matrix
list (set = set, get = get, setinverse = setinv, getinverse = getinv)
}

## *cacheSolve*: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
        
## this if loop says that if the matrix has been cached before (!is.null(i)), then the message "getting cached data" is returned.
## the inversed matrix will be returned by the return(i) command. 
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
        
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
