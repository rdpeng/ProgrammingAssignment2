

## The first function, makeCacheMatrix inverts a matrix, which is a list containing functions 
## set the matrix, get the matrix, set the inverse, get the inverse

makeCacheMatrix <- function(x = matrix()) {

    minv <- NULL
    set <- function(y) {
      x <<- y
      minv <<- NULL
    }
    get <- function() x
    setminverse <- function(minverse) minv <<- minverse
    getminverse <- function() minv
    list(set = set, get = get,
         setminverse = setminverse,
         getminverse = getminverse)
  }




##  this function inverts the original matrix to makeCacheMatrix functions

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv <- x$getminverse()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- minverse(data, ...)
  x$setminverse(minv)
  return(minv)
}
