## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# 1 . set the value of the matrix
# 2 . get the value of the matrix
# 3 . set the value of the inverse of the matrix
# 4 . get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)


}


## Write a short comment describing this function

# The second function returns the inverse of the given matrix. It examines if
# the inverse has already been calculated. If yes, it gets the result and skips the
# calculation. If no, it calculates the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(i != NULL) {
    message("getting cached data.")
    return(i)
  }
  dat <- x$get()
  i <- solve(dat)
  x$setinverse(i)
  i
}
