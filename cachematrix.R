## Put comments here that give an overall description of what your
## functions do
## Example function to cache potentially time-consuming computations
## Write a short comment describing this function
## Function makeCacheMatrix creates a matrix 
## and uses the solve function to find the inverse of this matrix 
## and can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
    x <<- y
    m <<- NULL
  }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## The function cacheSolve calculates the inverse of the matrix as defined above.
## But, it first checks the cache to see if the inverse has already been computed.
## If the inverse exists in the cache, it skip the computation and if not, it continues 
## to calcuate the inverse of the matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
