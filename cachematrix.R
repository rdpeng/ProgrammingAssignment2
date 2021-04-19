## Put comments here that give an overall description of what your
## functions do
# My functions caculate the inversion of given matrix
## Write a short comment describing this function
# This function can set and get values of matrix, also get and set inversion of matrix
makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setInverse <- function(new_inver) inver <<- new_inver
  getInverse <- function() inver
  list(get = get, set = set, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inver <- x$getInverse()
    if (!is.null(inver))
    {
      return(inver)
    }
  data <- x$get()
  inver <- solve(data, ...)
  x$setInver(inver)
  inver
  
        ## Return a matrix that is the inverse of 'x'
}
