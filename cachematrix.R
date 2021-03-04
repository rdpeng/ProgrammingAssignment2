## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special 
## “matrix”, which is really a list containing a function
## to: 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
#3 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special 
## “matrix” returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("retreiving cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
