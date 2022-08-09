## Put comments here that give an overall description of what your
## functions do

## The following function will proivde the inverse of a square matrix. It will only work with square matrix, which are invertible. Matrix that are not invertible,
## i.e. are not square matrix will not work

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function will create the inverse of the square matrix in case if the matrix inverse has not been calculated yet. If the inverse is already calculated, it
## will get it from cache and skip computation

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

## to check the function

makeMatrix(cachsolve(x=matrix(runif(100,0,1),nrow = 10,ncol = 10)))
